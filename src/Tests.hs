module Tests (singleCheck, fucheck, result2str) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)

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
    --Left arbExitCode -> return $ R.Exception (S.stateTestName state) Nothing R.Arb arbExitCode seed
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
        return $ R.SingleSuccess state
        else do
        shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
        return $ R.SingleFailure state shownInput


fucheck :: S.State -> IO R.Result
fucheck state
  | S.numSuccessTests state >= S.maxSuccessTests state =
      return $ R.Success
      { R.resultTestName = S.stateTestName state
      , R.numTests       = S.numSuccessTests state
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
                       , R.resultSeed     = S.getSeed state
                       }
    R.SingleException state stage ->
      return $ R.Exception { R.resultTestName = S.stateTestName state
                           , R.shownInput     = Nothing
                           , R.errorStage     = stage
                           , R.resultSeed     = S.getSeed state
                           }


result2str :: R.Result -> String
result2str (R.Success name numTests) =
  "Property " ++ name ++ " holds after " ++ show numTests ++ " tests"
result2str (R.Failure name Nothing seed) =
  "Property " ++ name ++ " failed on seed " ++ show seed
result2str (R.Failure name (Just (Right str)) _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (R.Failure name (Just (Left exitCode)) seed) =
  unlines $ ("Property " ++ name ++ " failed on seed " ++ show seed)
  : M.crashMessage name seed [("show",[("Exit code", show exitCode)])]
result2str (R.GaveUp name seed numTests) =
  "Property " ++ name ++ " gave up getting the input precondition to hold after " ++ show numTests ++ " tests."
result2str (R.Exception name Nothing stage seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage),[("Exit code", show $ R.exitCode stage)])]
result2str (R.Exception name (Just (Right input)) stage seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage), [ ("Input", input)
                                                            , ("Exit code", show $ R.exitCode stage)
                                                            ])]
result2str (R.Exception name (Just (Left showExitCode)) stage seed) =
  unlines $ M.crashMessage name seed [ ((R.stage2str stage),[("Exit code", show $ R.exitCode stage)])
                                     , ("show", [("Exit code", show showExitCode)])
                                     ]
