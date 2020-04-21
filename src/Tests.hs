module Tests (singleCheck, fucheck, result2str) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)

import qualified Result  as R
import qualified Message as M
import qualified State   as S

f *< a = f <*> pure a

singleCheck :: S.State -> IO R.Result
singleCheck state = do
  putStrLn $ show $ S.size state
  let seed = S.getSeed state
  eTestdata <- runExceptT $ (S.arbitrary state) (S.size state) seed
  case eTestdata of
    Left arbExitCode -> return $ R.Exception (S.stateTestName state) Nothing R.Arb arbExitCode seed
    Right testdata -> do
      eResult <- runExceptT $ (S.property state) testdata
      case eResult of
        Left propExitCode -> do
          shownInput <- sequence $ runExceptT <$> S.shower state *< testdata
          return $ R.Exception (S.stateTestName state) shownInput R.Prop propExitCode seed
        Right result -> do
          if result
            then return R.Success { R.resultTestName = S.stateTestName state
                                  , R.numTests = S.numSuccessTests state}
            else do
            shownInput2 <- sequence $ runExceptT <$> S.shower state *< testdata
            return $ R.Failure { R.resultTestName = S.stateTestName state
                             , R.shownInput     = shownInput2
                             , R.resultSeed     = seed
                             }

fucheck :: S.State -> IO R.Result
fucheck state
  | S.numSuccessTests state >= S.maxSuccessTests state =
      return $ R.Success
      { R.resultTestName = S.stateTestName state
      , R.numTests       = S.numSuccessTests state
      }
  | otherwise = do
  result <- singleCheck state
  case result of
    R.Success _ _ ->
      fucheck $ (snd (S.nextState state)) { S.numSuccessTests = S.numSuccessTests state + 1 }
    _       -> return result


result2str :: R.Result -> String
result2str (R.Success name numTests) = "Property " ++ name ++ " holds after " ++ show numTests ++ " tests"
result2str (R.Failure name Nothing seed) =
  "Property " ++ name ++ " failed on seed " ++ show seed
result2str (R.Failure name (Just (Right str)) _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (R.Failure name (Just (Left exitCode)) seed) =
  unlines $ ("Property " ++ name ++ " failed on seed " ++ show seed)
  : M.crashMessage name seed [("show",[("Exit code", show exitCode)])]
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
