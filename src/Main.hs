{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, removeFile)
import Control.Monad ((<=<))
import System.Environment(getArgs)
import qualified System.Process.Typed as TP
import System.Exit (ExitCode(ExitSuccess), exitSuccess, exitFailure)
import qualified System.Posix.DynamicLinker as DL
import System.Random (getStdGen, setStdGen, StdGen, randomIO)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import GHC.Err (errorWithoutStackTrace)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr)
import qualified Data.ByteString.Lazy.Char8 as BSL

import ParseFut ( FutFunNames
                , findTests
                , only
                , without
                , fixEntries
                , addStateGetters
                , stripComments
                , usableTest
                , filtersplit
                , ffTestName
                , arbFound
                , propFound
                , arbName
                , propName
                )


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

exitOnCompilationError exitCode msg =
  case exitCode of
    ExitSuccess -> return ()
    _           -> do
      BSL.putStrLn $ msg
      exitFailure

data WhichTests = All | Only [String] | Without [String]
filterTests All tests             = tests
filterTests (Only these) tests    = only these tests
filterTests (Without these) tests = without these tests

data Compiler = C | OpenCL deriving Show
futArgs args futFile =
  case compiler args of
    C      -> ["c",      "--library", "-o", tmpFile, futFile]
    OpenCL -> ["opencl", "--library", "-o", tmpFile, futFile]

gccArgs args =
  case compiler args of
    C      -> [tmpFile ++ ".c",             "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
    OpenCL -> [tmpFile ++ ".c", "-lOpenCL", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]


data Args = Args { file       :: String
                 , whichTests :: WhichTests
                 , compiler   :: Compiler
                 }

uniqueFile :: String -> String -> IO String
uniqueFile basename filetype = do
  -- what if fucheck is used from other dir?
  let name = basename ++ "." ++ filetype
  fileExists <- doesFileExist $ name
  if fileExists
  then do
    int <- randomIO :: IO Integer
    let newname = basename ++ show int
    uniqueFile newname filetype
  else return name



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
getFilter garbage = errorWithoutStackTrace ("Did not understand: " ++ unwords garbage)

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

-- assumes either arb or prop (or both) is missing
snitchOn tests =
  sequence $ map (\t ->
                     putStrLn
                     $ ffTestName t ++ " could not be tested because "
                     ++ if not (arbFound t || propFound t) then
                          arbName t ++ " and " ++ propName t ++ " weren't found"
                        else (if not $ arbFound t then arbName t else propName t) ++ " wasn't found"
                 ) tests

main :: IO ()
main = do
  args <- parseArgs <$> getArgs

  let filename = file args

  fileText <- readFile $ filename ++ ".fut"
  let alltests = findTests fileText
  let tests = filterTests (whichTests args) alltests

  let (goodtests,badtests) = filtersplit usableTest tests

  letThereBeDir tmpDir

  let alteredprogram = addStateGetters $ fixEntries goodtests $ fileText
  tmpFutFile <- uniqueFile filename "fut"
  writeFile tmpFutFile alteredprogram


  (futExitCode, futOut, futErr) <-
    TP.readProcess $ TP.proc "futhark" $ futArgs args tmpFutFile
  removeFile tmpFutFile
  exitOnCompilationError futExitCode futErr

  snitchOn badtests

  (gccExitCode, gccOut, gccErr) <-
    TP.readProcess $ TP.proc "gcc" $ gccArgs args
  exitOnCompilationError gccExitCode futErr

  dl <- DL.dlopen (tmpFile ++ ".so") [DL.RTLD_NOW] -- Read up on flags

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  states <- sequence $ map (mkDefaultState dl ctx) goodtests
  sequence_ $ map ((putStrLn . result2str) <=< fucheck) states
  freeFutContext dl ctx
  newFutFreeConfig dl cfg
  DL.dlclose dl
