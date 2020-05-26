{-# language ScopedTypeVariables #-}
module Tests (singleCheck, fucheck, result2str) where

import Data.List (foldl', sortOn)
import Control.Monad (join)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT, throwE)
import qualified Data.Map.Strict as MS

import qualified Result  as R
import qualified Message as M
import qualified State   as S
import FutInterface (CInt)

either2maybe (Right r) = Just r
either2maybe (Left _)  = Nothing

f *< a = f <*> pure a

singleCheck :: S.State -> IO R.SingleResult
singleCheck = useArb

useArb state = do
  eTestdata <- runExceptT $ S.runArbitrary state
  case eTestdata of
    Left arbExitCode -> return $ R.SingleException (R.Arb arbExitCode)
    Right testdata -> useCond state testdata


useCond state testdata =
  case S.condition state of
    Nothing -> useProp state testdata
    Just realcond -> do
      eCond <- runExceptT $ realcond testdata
      case eCond of
        Left condExitCode ->
          return $ R.SingleException (R.Cond condExitCode)
        Right cond -> do
          if not cond then
            return $ R.SingleGaveUp
            else useProp state testdata

useProp state testdata = do
  eResult <- runExceptT $ (S.property state) testdata
  case eResult of
    Left propExitCode ->
      return $ R.SingleException $ R.Prop propExitCode
    Right result -> do
      if result then do
        elabel <- sequence $ runExceptT <$> S.labeler state *< testdata
        case elabel of
          Nothing -> do
            return $ R.SingleSuccess Nothing
          Just (Right label) -> do
            -- move change of state out to fucheck
            return $ R.SingleSuccess (Just label)
          Just (Left labelExitCode) ->
            return $ R.SingleException $ R.Label labelExitCode
        else return $ R.SingleFailure


fucheck :: S.State -> IO R.Result
fucheck state
  | S.numSuccessTests state >= S.maxSuccessTests state =
      return $ R.Success
      { R.resultTestName = S.stateTestName state
      , R.numTests       = S.numSuccessTests state
      , R.resultLabels   = S.labels state
      }

  | S.numRecentlyDiscardedTests state >= S.maxDiscardedRatio state =
    return $ R.GaveUp { R.resultTestName = S.stateTestName state
                      , R.resultSeed     = S.getSeed state
                      , R.numTests       = S.numSuccessTests state
                      }

  | otherwise = do
  result <- singleCheck state
  case result of
    R.SingleSuccess label ->
      fucheck $ state { S.randomSeed = S.nextGen state
                      , S.numSuccessTests = S.numSuccessTests state + 1
                      , S.numRecentlyDiscardedTests = 0
                      , S.labels = (MS.alter alterFun <$> label) <*> S.labels state
                      }
      where
        alterFun (Nothing) = Just 1
        alterFun (Just n)  = Just $ n + 1

    R.SingleGaveUp ->
      fucheck $ state { S.randomSeed = S.nextGen state
                      , S.numDiscardedTests = S.numDiscardedTests state + 1
                      , S.numRecentlyDiscardedTests = S.numRecentlyDiscardedTests state + 1
                      }

    R.SingleFailure -> do
      s <- runExceptT shownInput
      case s of
        Right sjov ->
          return $ R.Failure { R.resultTestName = S.stateTestName state
                             , R.shownInput     = sjov
                             , R.resultSeed     = S.getSeed state
                             }
        Left exitCode ->
          return $ R.Exception { R.resultTestName = S.stateTestName state
                               , R.shownInput     = Nothing
                               , R.errorStage     = R.Show exitCode
                               , R.resultSeed     = S.getSeed state
                               }

    R.SingleException stage -> do
      s <- runExceptT shownInput
      return $ R.Exception { R.resultTestName = S.stateTestName state
                           , R.shownInput     = join $ either2maybe s
                           , R.errorStage     = stage
                           , R.resultSeed     = S.getSeed state
                           }

    where
      shownInput :: ExceptT CInt IO (Maybe String)
      shownInput =
        case S.shower state of
          Nothing -> return Nothing
          Just s -> Just <$> (s =<< S.runArbitrary state)


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
