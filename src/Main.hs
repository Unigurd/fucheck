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
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr)

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

data Compiler = C | OpenCL deriving Show
futArgs args =
  case compiler args of
    C      -> ["c",      "--library", "-o", tmpFile, (file args) ++ ".fut"]
    OpenCL -> ["opencl", "--library", "-o", tmpFile, (file args) ++ ".fut"]

gccArgs args =
  case compiler args of
    C      -> [tmpFile ++ ".c",             "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
    OpenCL -> [tmpFile ++ ".c", "-lOpenCL", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]


data Args = Args { file       :: String
                 , whichTests :: WhichTests
                 , compiler   :: Compiler
                 }

tmpDir  = "/tmp/fucheck/"
tmpFile = tmpDir ++ "fucheck-tmp-file"

getCompiler ("opencl":rest) = (OpenCL, rest)
getCompiler ("c":rest)      = (C, rest)
getCompiler rest            = (C, rest)

getFilename [] = errorWithoutStackTrace "Missing file argument"
getFilename (filename: rest) = (filename, rest)

getFilter ["--without"] = errorWithoutStackTrace "Missing test arguments to --without"
getFilter ["--only"]    = errorWithoutStackTrace "Missing test arguments to --only"
getFilter ("--without":tests) = Without tests
getFilter ("--only":tests)    = Only tests
getFilter []                  = All
getFilter garbage       = errorWithoutStackTrace ("Did not understand: " ++ unwords garbage)


parseArgs :: [String] -> Args
parseArgs strArgs = args
  where
    (compiler, rest0) = getCompiler strArgs
    (filename, rest1) = getFilename rest0
    testFilter        = getFilter rest1
    args              = Args { file = filename
                             , whichTests = testFilter
                             , compiler = compiler
                             }

main :: IO ()
main = do
  args <- parseArgs <$> getArgs

  let filename = file args

  fileText <- readFile $ filename ++ ".fut"
  let testNames = filterTests (whichTests args) (findTests fileText)

  letThereBeDir tmpDir

  (futExitCode, futOut, futErr) <-
    TP.readProcess $ TP.proc "futhark" $ futArgs args
  exitOnCompilationError futExitCode $ filename ++ ".fut"

  (gccExitCode, gccOut, gccErr) <-
    TP.readProcess $ TP.proc "gcc" $ gccArgs args
  exitOnCompilationError gccExitCode $ "generated C file"

  dl <- DL.dlopen (tmpFile ++ ".so") [DL.RTLD_NOW] -- Read up on flags

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  states <- sequence $ map (mkDefaultState dl ctx) testNames
  sequence_ $ map ((putStrLn . result2str) <=< fucheck) states
  freeFutContext dl ctx
  newFutFreeConfig dl cfg
  DL.dlclose dl
