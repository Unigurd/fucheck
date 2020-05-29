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
import Data.String (fromString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (foldl', break)
import Debug.Trace(trace)

import CLI ( Args(..)
           , Compiler(..)
           , Action(..)
           , parseArgs
           , file
           , whichTests
           , filterTests
           , futArgs
           , gccArgs
           )

import ParseFut ( FutFunNames
                , findTests
                , only
                , without
                , usableTest
                , filtersplit
                , ffTestName
                , arbFound
                , propFound
                , arbName
                , propName
                )

import WriteFut ( fixEntries
                , combineFutFuns
                , addStateGetters
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


-- assumes either arb or prop (or both) is missing
snitchOn tests =
  sequence $ map (\t ->
                     putStrLn
                     $ ffTestName t ++ " could not be tested because "
                     ++ if not (arbFound t || propFound t) then
                          arbName t ++ " and " ++ propName t ++ " weren't found"
                        else (if not $ arbFound t then arbName t else propName t) ++ " wasn't found"
                 ) tests
tmpDir  = "/tmp/fucheck/"
tmpFile = tmpDir ++ "fucheck-tmp-file"

uniqueFile :: String -> IO String
uniqueFile filename = uncurry uniqueHelper $ break (=='.') filename
  where
    uniqueHelper basename filetype = do
      -- what if fucheck is used from other dir?
      let name = basename ++ filetype
      fileExists <- doesFileExist $ name
      if fileExists
        then do
        int <- randomIO :: IO Integer
        let newname = basename ++ show int
        uniqueHelper newname filetype
        else return name

main :: IO ()
main = do
  -- commandline args
  eArgs <- parseArgs <$> getArgs
  args <- case eArgs of
        Left errMsg -> do
          putStrLn errMsg
          exitFailure
        Right args -> return args

  -- preprocessing
  let filename = file args

  fileText <- readFile $ filename
  let alltests = findTests fileText
  let tests = filterTests (whichTests args) alltests

  let (goodtests,badtests) = filtersplit usableTest tests

  letThereBeDir tmpDir

  let alteredprogram =
        unlines [ fixEntries goodtests fileText
                , addStateGetters
                ]
  tmpFutFile <- uniqueFile filename
  writeFile tmpFutFile alteredprogram
  if action args == SaveFile then exitSuccess else return ()

  -- compilation
  (whichFutharkExitCode, _, _) <-
    TP.readProcess $ TP.proc "which" ["futhark"]
  exitOnCompilationError whichFutharkExitCode $ fromString "futhark does not seem to be installed"

  (whichGCCExitCode, _, _) <-
    TP.readProcess $ TP.proc "which" ["gcc"]
  exitOnCompilationError whichGCCExitCode $ fromString "gcc does not seem to be installed"

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
