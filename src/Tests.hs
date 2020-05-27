{-# language ScopedTypeVariables #-}
module Tests (fucheck, result2str) where

import Data.List (foldl', sortOn)
import Data.Either (either)
import Control.Monad (join)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT, throwE, except)
import qualified Data.Map.Strict as MS

import qualified Result  as R
import Result (Stage)
import qualified Message as M
import qualified State   as S
import FutInterface (CInt, Ptr, FutharkTestData)

either2maybe (Right r) = Just r
either2maybe (Left _)  = Nothing

f *< a = f <*> pure a


-- change name from 'run'
-- Nothing if no inputs passed the condition. Just Left if something crashed
runGen :: S.State -> IO (S.State, Maybe (Either Stage (Ptr FutharkTestData)))
--runGen :: S.State -> (S.State, ExceptT Stage IO (Maybe (Ptr futharkTestData)))
runGen state
  -- Max number of attempts at generating testdata reached
  | S.numRecentlyDiscardedTests state >= S.maxDiscardedRatio state = return (state, Nothing)

  -- Try to generate testdata
  | otherwise = do
      eTestdata <- runExceptT $ S.runArbitrary state
      case (S.condition state, eTestdata) of
        --(Nothing, _) -> return (state, return $ onLeft (\errcode -> R.Arb errcode) eTestdata)

        -- Arb errored
        (_, Left arbExitCode) ->
          return (state, return $ Left arbExitCode)

        -- No condition, so just return testdata
        (Nothing, Right testdata) ->
          return (state, Just $ return testdata)

        -- Condition is specified
        (Just cond, Right testdata) -> do
          condResult <- runExceptT $ cond testdata
          case condResult of

            -- Condition errored
            Left errorcode -> return $ (state, Just $ Left errorcode)
            -- Condition didn't crash
            Right b ->
              if b
              -- testdata fulfilled condition
              then return  (state { S.numRecentlyDiscardedTests = 0 }, return $ return testdata)
              -- testdata didn't fulfill condition, so try again
              else runGen $ state { S.numRecentlyDiscardedTests =
                                      S.numRecentlyDiscardedTests state + 1
                                  , S.randomSeed = S.nextGen state
                                  }
        where
          -- Why doesn't onLeft work in the commented line above?
          onLeft f (Left l) = Left $ f l
          onLeft _ rightval  = rightval

foundException state exitcode string =
  R.Exception { R.resultTestName = S.stateTestName state
              , R.shownInput     = string
              , R.errorStage     = exitcode
              , R.resultSeed     = S.getSeed state
              }

shownInput :: S.State -> ExceptT Stage IO (Maybe String)
shownInput state =
  case S.shower state of
    Nothing -> return Nothing
    Just s -> Just <$> (s =<< S.runArbitrary state)

ignoreShowException = join . either2maybe

fucheck :: S.State -> IO R.Result
fucheck state = do
  finalResult <- runExceptT $ fucheckRec state
  case finalResult of
    Right result -> return result
    Left  errVal -> do
      s <- runExceptT $ shownInput state
      return $ foundException state errVal $ ignoreShowException s

  where
    fucheckRec :: S.State -> ExceptT Stage IO R.Result
    fucheckRec state
      -- Passed all tests
      | S.numSuccessTests state >= S.maxSuccessTests state =
        return $ R.Success
        { R.resultTestName = S.stateTestName state
        , R.numTests       = S.numSuccessTests state
        , R.resultLabels   = S.labels state
        }

      -- otherwise
      | otherwise = do
          (state, mTestdata) <- ExceptT $ return <$> runGen state
          case mTestdata of

            -- Could not generate data that passed the precondition
            Nothing ->
              return $ R.GaveUp { R.resultTestName = S.stateTestName state
                                , R.resultSeed     = S.getSeed state
                                , R.numTests       = S.numSuccessTests state
                                }

            Just eTestdata -> do
              testdata <- except eTestdata
              result <- S.property state testdata
              if result then do
                case S.labeler state *< testdata of
                  Nothing ->
                    fucheckRec $ state { S.randomSeed = S.nextGen state
                                    , S.numSuccessTests = S.numSuccessTests state + 1
                                    , S.numRecentlyDiscardedTests = 0
                                    }
                  -- Labeling succeeded
                  Just eLabel -> do
                    label <- eLabel
                    fucheckRec $ state { S.randomSeed = S.nextGen state
                                    , S.numSuccessTests = S.numSuccessTests state + 1
                                    , S.numRecentlyDiscardedTests = 0
                                    , S.labels = (MS.alter alterFun <$> Just label) <*> S.labels state
                                    }
                      where
                        alterFun (Nothing) = Just 1
                        alterFun (Just n)  = Just $ n + 1

                else do
                s <- shownInput state
                return $ R.Failure { R.resultTestName = S.stateTestName state
                                   , R.shownInput     = s
                                   , R.resultSeed     = S.getSeed state
                                   }


result2str :: R.Result -> String
result2str (R.Success name numTests Nothing) =
  "Property " ++ name ++ " holds after " ++ show numTests ++ " tests"
result2str (R.Success name numTests (Just labelsMap)) =
  unlines (("Property " ++ name ++ " holds after " ++ show numTests ++ " tests") : labelStrs)
   where
     labelsList = sortOn (length . fst) $ MS.toList labelsMap
     (labels,freqs)    = unzip labelsList
     (total :: Float )     = fromIntegral $ foldl' (+) 0 $ MS.elems labelsMap
     percentages =
       (\elem -> show (round ((elem / total) * 100))) -- div by zero?
       <$> fromIntegral
       <$> freqs
     labelStrs = M.formatMessages "% " $ zip percentages labels
result2str (R.Failure name Nothing seed) =
  "Property " ++ name ++ " failed on seed " ++ show seed
result2str (R.Failure name (Just str) _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (R.GaveUp name seed numTests) =
  "Property " ++ name ++ " gave up getting the input precondition to hold after " ++ show numTests ++ " tests."
result2str (R.Exception name Nothing stage seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage),[("Exit code", show $ R.exitCode stage)])]
result2str (R.Exception name (Just input) stage seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage), [ ("Input", input)
                                                            , ("Exit code", show $ R.exitCode stage)
                                                            ])]
