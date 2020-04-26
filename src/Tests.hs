{-# language ScopedTypeVariables #-}
module Tests (singleCheck, fucheck, result2str) where

import Data.List (foldl', sortOn)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import qualified Data.Map.Strict as MS

import qualified Result  as R
import qualified Message as M
import qualified State   as S

f *< a = f <*> pure a

singleCheck :: S.State -> IO R.SingleResult
singleCheck = useArb

useArb state = do
  let seed = S.getSeed state
  eTestdata <- runExceptT $ (S.arbitrary state) (S.size state) seed
  case eTestdata of
    Left arbExitCode -> return $ R.SingleException state (R.Arb arbExitCode)
    Right testdata -> useCond state testdata


useCond state testdata =
  case S.condition state of
    Nothing -> useProp state testdata
    Just realcond -> do
      eCond <- runExceptT $ realcond testdata
      case eCond of
        Left condExitCode -> do
          shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
          return $ R.SingleException state (R.Cond condExitCode)
        Right cond -> do
          if not cond then
            return $ R.SingleGaveUp state
            else useProp state testdata

useProp state testdata = do
  eResult <- runExceptT $ (S.property state) testdata
  case eResult of
    Left propExitCode -> do
      shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
      return $ R.SingleException state $ R.Prop propExitCode
    Right result -> do
      if result then do
        elabel <- sequence $ runExceptT <$> S.labeler state *< testdata
        case elabel of
          Nothing -> do
            return $ R.SingleSuccess state
          Just (Right label) -> do
            return $ R.SingleSuccess $ state { S.labels = MS.alter alterFun label <$> S.labels state }
            where
              alterFun (Nothing) = Just 1
              alterFun (Just n)  = Just $ n + 1
          Just (Left labelExitCode) ->
            return $ R.SingleException state $ R.Label labelExitCode
        else do
        shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
        return $ R.SingleFailure state shownInput


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
    R.SingleSuccess state ->
      fucheck $ state { S.randomSeed = S.nextGen state
                      , S.numSuccessTests = S.numSuccessTests state + 1
                      , S.numRecentlyDiscardedTests = 0
                      }
    R.SingleGaveUp state ->
      fucheck $ state { S.randomSeed = S.nextGen state
                      , S.numDiscardedTests = S.numDiscardedTests state + 1
                      , S.numRecentlyDiscardedTests = S.numRecentlyDiscardedTests state + 1
                      }
    R.SingleFailure state shownInput ->
      return $ R.Failure { R.resultTestName = S.stateTestName state
                       , R.shownInput     = shownInput
                       , R.resultSize     = S.size state
                       , R.resultSeed     = S.getSeed state
                       }
    R.SingleException state stage ->
      return $ R.Exception { R.resultTestName = S.stateTestName state
                           , R.shownInput     = Nothing
                           , R.errorStage     = stage
                           , R.resultSeed     = S.getSeed state
                           , R.resultSize     = S.size state
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
       (\elem -> show (round ((elem / total) * 100)))
       <$> fromIntegral
       <$> freqs
     labelStrs = M.formatMessages "% " $ zip percentages labels
result2str (R.Failure name Nothing size seed) =
  "Property " ++ name ++ " failed with size " ++ show size ++ " and seed " ++ show seed
result2str (R.Failure name (Just (Right str)) _ _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (R.Failure name (Just (Left exitCode)) size seed) =
  unlines $ ("Property " ++ name ++ " failed on seed " ++ show seed)
  : M.crashMessage name size seed [("show",[("Exit code", show exitCode)])] -- size -1 skal aendres
result2str (R.GaveUp name seed numTests) =
  "Property " ++ name ++ " gave up getting the input precondition to hold after " ++ show numTests ++ " tests."
result2str (R.Exception name Nothing stage size seed) =
  unlines $ M.crashMessage name size seed
  [((R.stage2str stage),[("Exit code", show $ R.exitCode stage)])]
result2str (R.Exception name (Just (Right input)) stage size seed) =
  unlines $ M.crashMessage name size seed
  [((R.stage2str stage), [ ("Input", input)
                         , ("Exit code", show $ R.exitCode stage)
                         ])]
result2str (R.Exception name (Just (Left showExitCode)) stage size seed) =
  unlines $ M.crashMessage name size seed
  [ ((R.stage2str stage),[("Exit code", show $ R.exitCode stage)])
  , ("show", [("Exit code", show showExitCode)])
  ]
