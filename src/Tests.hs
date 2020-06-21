-- Copyright (C) Sigurd Dam Sonniks

-- This file is part of Fucheck.

--     Fucheck is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     Fucheck is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with Fucheck.  If not, see <https://www.gnu.org/licenses/>.

{-# language ScopedTypeVariables #-}
module Tests (fucheck, result2str) where

import Data.List (foldl', sortOn)
import Data.Either (either)
import Control.Monad (join)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT, throwE, except, mapExceptT)
import qualified Data.Map.Strict as MS

import qualified Result  as R
import Result (Stage)
import qualified Message as M
import qualified State   as S
import FutInterface (CInt, Ptr, FutharkTestData)

either2maybe (Right r) = Just r
either2maybe (Left _)  = Nothing

-- apply a Maybe function
f *< a = f <*> pure a


-- Tries to generate some test data (Right True) until an exception occurs (Left Stage)
-- or the max number of tries has been reached (Right False)
-- change name from 'run'
loopCond :: S.State -> IO (S.State, Either Stage Bool)
loopCond state
  -- Max number of attempts at generating testdata reached
  | S.numRecentlyDiscardedTests state >= S.maxDiscardedRatio state = return (state, Right False)

  -- Try to generate testdata
  | otherwise = do
      eitherbool <- S.runCond state
      case eitherbool of
        Nothing   -> return (state, Right True)
        Just (Left stage)  -> return (state, Left stage)
        Just (Right True)  -> return (state { S.numRecentlyDiscardedTests = 0 }, Right True)
        Just (Right False) ->
          loopCond $ state { S.numRecentlyDiscardedTests =
                             S.numRecentlyDiscardedTests state + 1
                         , S.randomSeed = S.nextGen state
                         }

-- reports a futhark exception
foundException state exitcode string =
  R.Exception { R.resultTestName = S.stateTestName state
              , R.shownInput     = string
              , R.errorStage     = exitcode
              , R.resultSeed     = S.getSeed state
              }

-- Ignores if show throws exceptions.
-- used when there already was an exception within futhark
ignoreShowException str = join $ either2maybe <$> str

mapLeft f e = ExceptT $ fmap (\x -> case x of Left a -> Left (f a) ; Right a -> Right a) $ runExceptT e

-- main loop
fucheck :: S.State -> IO R.Result
fucheck state = fucheckRec state
  where
    fucheckRec :: S.State -> IO R.Result
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
          (state, condResult) <- loopCond state
          case condResult of
            -- If precondition crashed
            Left stage  -> do
              s <- S.runShow state
              return $ foundException state stage $ ignoreShowException s

            -- If no data was found to hold precondition
            Right False ->
              return $ R.GaveUp { R.resultTestName = S.stateTestName state
                                , R.resultSeed     = S.getSeed state
                                , R.numTests       = S.numSuccessTests state
                                }

            -- precondition satisfied
            Right True -> do
              propResult <- runExceptT $ S.runProp state
              case propResult of
                -- Property crashed
                Left stage -> do
                  s <- S.runShow state
                  return $ foundException state stage $ ignoreShowException s
                -- Property failed
                Right False -> do
                  showResult <- S.runShow state
                  case showResult of
                    -- No show function
                    Nothing ->
                      return $ R.Failure { R.resultTestName = S.stateTestName state
                                         , R.shownInput     = Nothing
                                         , R.resultSeed     = S.getSeed state
                                         }
                    -- Show crashed
                    Just (Left stage) ->
                      return $ foundException state stage Nothing
                    -- Counterexample shown
                    Just (Right str) ->
                      return $ R.Failure { R.resultTestName = S.stateTestName state
                                         , R.shownInput     = Just str
                                         , R.resultSeed     = S.getSeed state
                                         }

                -- Property holds
                Right True -> do
                  newLabel <- S.runLabels state
                  case newLabel of
                    -- No labeling
                    Nothing ->
                      fucheckRec $ state { S.randomSeed = S.nextGen state
                                         , S.numSuccessTests = S.numSuccessTests state + 1
                                         , S.numRecentlyDiscardedTests = 0
                                         }

                    -- Labeling crashed
                    Just (Left stage) -> do
                      s <- S.runShow state
                      return $ foundException state stage $ ignoreShowException s

                    -- Labeling succeeded
                    Just (Right label) ->
                      fucheckRec $ state { S.randomSeed = S.nextGen state
                                         , S.numSuccessTests = S.numSuccessTests state + 1
                                         , S.numRecentlyDiscardedTests = 0
                                         , S.labels =
                                             (MS.alter alterFun <$> Just label) <*> S.labels state
                                         }
                      where
                        alterFun (Nothing) = Just 1
                        alterFun (Just n)  = Just $ n + 1


-- turns the result into a string to print
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
