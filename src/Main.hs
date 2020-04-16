{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Directory (createDirectory, doesDirectoryExist)
import Control.Monad ((<=<))
import System.Environment(getArgs)
import qualified System.Process.Typed as TP
import System.Exit (ExitCode(ExitSuccess), exitSuccess, exitFailure)
import qualified System.Posix.DynamicLinker as DL
import System.Random (getStdGen, StdGen)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)

import FutInterface ( newFutConfig
                    , newFutContext
                    , newFutFreeConfig
                    , freeFutContext
                    , CInt
                    , Ptr
                    , FutharkTestData
                    )
import FutFuns (findTests, loadFutFuns, ffTestName)
import Tests (fucheck, result2str)
import State (mkDefaultState)

uncurry3 f (a,b,c) = f a b c


headWithDefault def []     = def
headWithDefault _ (head:_) = head






letThereBeDir dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then createDirectory dir
    else return ()

exitOnCompilationError exitCode filename =
  case exitCode of
    ExitSuccess -> return ()
    _           -> do
      putStrLn $ "Could not compile " ++ filename
      exitFailure

testIOprep dl ctx test = do
  gen <- getStdGen
  futFuns <- loadFutFuns dl ctx test
  return (ffTestName test, gen,futFuns)


main :: IO ()
main = do
  args <- getArgs
  --case compare (length args) 1 of
  --  LT -> do
  --    putStrLn "Give test file as argument"
  --    exitSuccess
  --  GT -> do
  --    putStrLn "Only accepts one argument; the test file"
  --    exitSuccess
  --  EQ -> return ()

  let filename = headWithDefault "src/futs/fucheck" args
  let tmpDir = "/tmp/fucheck/"
  let tmpFile = tmpDir ++ "fucheck-tmp-file"

  fileText <- readFile $ filename ++ ".fut"
  let testNames = findTests fileText

  letThereBeDir tmpDir

  (futExitCode, futOut, futErr) <-
    TP.readProcess $ TP.proc "futhark" ["c", "--library", "-o", tmpFile, filename ++ ".fut"]
  exitOnCompilationError futExitCode $ filename ++ ".fut"

  (gccExitCode, gccOut, gccErr) <-
    TP.readProcess $ TP.proc "gcc" [tmpFile ++ ".c", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
  exitOnCompilationError gccExitCode $ "generated C file"

  dl <- DL.dlopen (tmpFile ++ ".so") [DL.RTLD_NOW] -- Read up on flags

  let firstTest = head testNames

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  ioPrep <- sequence $ map (testIOprep dl ctx) testNames
  let states = map (uncurry3 mkDefaultState) ioPrep
  sequence_ $ map ((putStrLn . result2str) <=< fucheck) states
  freeFutContext dl ctx
  newFutFreeConfig dl cfg
  DL.dlclose dl
