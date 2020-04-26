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
import GHC.Err (errorWithoutStackTrace)

import ParseFut (FutFunNames, findTests, only, without)

import FutInterface ( newFutConfig
                    , newFutContext
                    , newFutFreeConfig
                    , freeFutContext
                    , CInt
                    , Ptr
                    , FutharkTestData
                    )
import Tests (fucheck, result2str)
import State (mkDefaultState)

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

data WhichTests = All | Only [String] | Without [String]
filterTests All tests             = tests
filterTests (Only these) tests    = only these tests
filterTests (Without these) tests = without these tests

data Args = Args { file :: String
                 , whichTests :: WhichTests
                 }
parseArgs :: [String] -> Args
parseArgs [] = errorWithoutStackTrace "Missing file argument"
parseArgs [file] = Args file All
parseArgs [file, "--only"] = errorWithoutStackTrace "Missing test arguments to --only"
parseArgs (file : "--only" : tests) = Args file $ Only tests
parseArgs [file, "--without"] = errorWithoutStackTrace "Missing test arguments to --without"
parseArgs (file : "--without" : tests) = Args file $ Without tests


main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  --case compare (length args) 1 of
  --  LT -> do
  --    putStrLn "Give test file as argument"
  --    exitSuccess
  --  GT -> do
  --    putStrLn "Only accepts one argument; the test file"
  --    exitSuccess
  --  EQ -> return ()

  let filename = file args
  let tmpDir = "/tmp/fucheck/"
  let tmpFile = tmpDir ++ "fucheck-tmp-file"

  fileText <- readFile $ filename ++ ".fut"
  let testNames = filterTests (whichTests args) (findTests fileText)

  letThereBeDir tmpDir

  (futExitCode, futOut, futErr) <-
    TP.readProcess $ TP.proc "futhark" ["c", "--library", "-o", tmpFile, filename ++ ".fut"]
  exitOnCompilationError futExitCode $ filename ++ ".fut"

  (gccExitCode, gccOut, gccErr) <-
    TP.readProcess $ TP.proc "gcc" [tmpFile ++ ".c", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
  exitOnCompilationError gccExitCode $ "generated C file"

  dl <- DL.dlopen (tmpFile ++ ".so") [DL.RTLD_NOW] -- Read up on flags

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  states <- sequence $ map (mkDefaultState dl ctx) testNames
  sequence_ $ map ((putStrLn . result2str) <=< fucheck) states
  freeFutContext dl ctx
  newFutFreeConfig dl cfg
  DL.dlclose dl
