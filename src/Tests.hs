module Tests (singleCheck, fucheck, result2str) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)

import qualified Result  as R
import qualified Message as M
import qualified State   as S

f *< a = f <*> pure a

singleCheck :: S.State -> IO R.Result
singleCheck state = do
  let seed = S.getSeed state
  eTestdata <- runExceptT $ (S.arbitrary state) (S.size state) seed
  case eTestdata of
    Left arbExitCode -> return $ R.Exception (S.stateTestName state) Nothing R.Arb arbExitCode seed
    Right testdata -> useCond state testdata

useCond state testdata =
  case S.condition state of
    Nothing -> useProp state testdata
    Just realcond -> do
      eCond <- runExceptT $ realcond testdata
      case eCond of
        Left condExitCode -> do
          shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
          return $ R.Exception (S.stateTestName state) shownInput R.Cond condExitCode $ S.getSeed state
        Right cond -> do
          if not cond then
            return $ R.GaveUp { R.resultTestName = S.stateTestName state
                              , R.resultSeed     = S.getSeed state
                              , R.numTests       = S.numSuccessTests state
                              }
            else useProp state testdata

useProp state testdata = do
  eResult <- runExceptT $ (S.property state) testdata
  case eResult of
    Left propExitCode -> do
      shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
      return $ R.Exception (S.stateTestName state) shownInput R.Prop propExitCode (S.getSeed state)
    Right result -> do
      if result then do
        return R.Success { R.resultTestName = S.stateTestName state
                         , R.numTests = S.numSuccessTests state
                         }
        else do
        shownInput2 <- sequence $ runExceptT <$> S.shower state *< testdata
        return $ R.Failure { R.resultTestName = S.stateTestName state
                           , R.shownInput     = shownInput2
                           , R.resultSeed     = S.getSeed state
                           }

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
    R.Success _ _ ->
      fucheck $ (snd (S.nextState state))
      { S.numSuccessTests = S.numSuccessTests state + 1
      , S.numRecentlyDiscardedTests = 0
      }
    R.GaveUp _ _ _ ->
      fucheck $ (snd (S.nextState state))
      { S.numDiscardedTests = S.numDiscardedTests state + 1
      , S.numRecentlyDiscardedTests = S.numRecentlyDiscardedTests state + 1
      }
    _ -> return result


result2str :: R.Result -> String
result2str (R.Success name numTests) = "Property " ++ name ++ " holds after " ++ show numTests ++ " tests"
result2str (R.Failure name Nothing seed) =
  "Property " ++ name ++ " failed on seed " ++ show seed
result2str (R.Failure name (Just (Right str)) _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (R.Failure name (Just (Left exitCode)) seed) =
  unlines $ ("Property " ++ name ++ " failed on seed " ++ show seed)
  : M.crashMessage name seed [("show",[("Exit code", show exitCode)])]
result2str (R.GaveUp name seed numTests) =
  "Property " ++ name ++ " gave up getting the input precondition to hold after " ++ show numTests ++ " tests."
result2str (R.Exception name Nothing stage exitCode seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage),[("Exit code", show exitCode)])]
result2str (R.Exception name (Just (Right input)) stage exitCode seed) =
  unlines $ M.crashMessage name seed [((R.stage2str stage), [ ("Input", input)
                                                        , ("Exit code", show exitCode)
                                                        ])]

result2str (R.Exception name (Just (Left showExitCode)) stage exitCode seed) =
  unlines $ M.crashMessage name seed [ ((R.stage2str stage),[("Exit code", show exitCode)])
                                   , ("show", [("Exit code", show showExitCode)])
                                   ]
