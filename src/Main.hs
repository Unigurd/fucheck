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

data WhichTests = All | Only [String] | Without [String]
filterTests All tests             = tests
filterTests (Only these) tests    = only these tests
filterTests (Without these) tests = without these tests

data Compiler = C | OpenCL deriving Show
futArgs args futFile =
  case compiler args of
    C      -> ["c",      "--library", "-o", tmpFile, futFile]
    OpenCL -> ["opencl", "--library", "-o", tmpFile, futFile]

data Action = Run | SaveFile deriving Eq
gccArgs args =
  case compiler args of
    C      -> [tmpFile ++ ".c",             "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
    OpenCL -> [tmpFile ++ ".c", "-lOpenCL", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]


data Args = Args { file       :: String
                 , whichTests :: WhichTests
                 , compiler   :: Compiler
                 , action     :: Action
                 }

defaultArgs file = Args { file       = file
                        , whichTests = All
                        , compiler   = C
                        , action     = Run
                        }

data ArgsGiven = ArgsGiven { whichTestsGiven :: Bool
                           , compilerGiven   :: Bool
                           , actionGiven     :: Bool
                           }
defaultArgsGiven = ArgsGiven { whichTestsGiven = False
                             , compilerGiven   = False
                             , actionGiven     = False
                             }

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


tmpDir  = "/tmp/fucheck/"
tmpFile = tmpDir ++ "fucheck-tmp-file"

getCompiler ("opencl":rest) = (OpenCL, rest)
getCompiler ("c":rest)      = (C, rest)
getCompiler rest            = (C, rest)

getFilename [] = errorWithoutStackTrace "Missing file argument"
getFilename (filename:rest) = (filename, rest)

getFilter ["--without"] = errorWithoutStackTrace "Missing test arguments to --without"
getFilter ["--only"]    = errorWithoutStackTrace "Missing test arguments to --only"
getFilter ("--without":tests) = Without tests
getFilter ("--only":tests)    = Only tests
getFilter []                  = All
getFilter garbage = errorWithoutStackTrace ("Did not understand: " ++ unwords garbage)

isFlag ('-':_) = True
isFlag _       = False

stdErrMsg = "Something went wrong parsing cmd line args"

separateFlags :: [String] -> [[String]]
separateFlags args = helper args []
  where
  helper [] acc = reverse <$> acc
  helper (a:as) [] = helper as [[a]] -- Might add non-flag as flag here
  helper (a:as) acc@(head_acc:tail_acc) =
    if isFlag a then
      helper as ([a]:acc)
    else
      helper as ((a:head_acc):tail_acc)


compilerFlags   = ["c","--c","opencl","--opencl"]
testFilterFlags = ["--only","--without"]
actionFlags     = ["--tmp", "--run"]

matches :: String -> [String] -> Bool
flag `matches` flagList = or $ (flag ==) <$> flagList

compFlag2comp "c"        = Right C
compFlag2comp "--c"      = Right C
compFlag2comp "opencl"   = Right OpenCL
compFlag2comp "--opencl" = Right OpenCL
compFlag2comp badFlag = Left $ "unrecognized compiler flag: " ++ badFlag

testFilterFlag2testFilter ("--only":args)    = Right $ Only args
testFilterFlag2testFilter ("--without":args) = Right $ Without args
testFilterFlag2testFilter _ = Left stdErrMsg

actionFlag2action "--tmp" = Right SaveFile
actionFlag2action "--run" = Right Run
actionFlag2action _ = Left stdErrMsg

parseCompilerFlag [] _ = Left stdErrMsg
parseCompilerFlag [flag] (args,given) =
  if compilerGiven given then
    Left "Compiler was specified more than once"
  else do
    comp <- compFlag2comp flag
    Right $ (args { compiler = comp }, given { compilerGiven = True })
parseCompilerFlag (flag:_) _ = Left $ "Compiler arg " ++ flag ++ " does not take arguments"

parseTestFilter [] _ = Left stdErrMsg
parseTestFilter flag (args,given) =
  if whichTestsGiven given then
    Left "Test filtering was specified more than once"
  else do
    testFilter <- testFilterFlag2testFilter flag
    Right $ (args { whichTests = testFilter }, given { whichTestsGiven = True })

--parseAction :: [String] -> 
parseAction [] _ = Left stdErrMsg
parseAction [flag] (args,given) =
  if actionGiven given then
    Left "Action was specified more than once"
  else do
    action <- actionFlag2action flag
    Right $ (args { action = action }, given { actionGiven = True })

-- shouldn't be named 'run'
runFlag :: [String] -> Either String (Args,ArgsGiven) -> Either String (Args,ArgsGiven)
runFlag [] _ = Left stdErrMsg
runFlag (flag:flagArgs) eAcc = do
  acc <- eAcc
  if flag `matches` compilerFlags
    then parseCompilerFlag (flag:flagArgs) acc
    else if flag `matches` testFilterFlags
    then parseTestFilter (flag:flagArgs) acc
    else if flag `matches` actionFlags
    then parseAction (flag:flagArgs) acc
    else Left $ "Did not recognize flag " ++ flag

parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No test file specified"
parseArgs allStrArgs = parsedArgs
  where
    file    = last allStrArgs
    strArgs = init allStrArgs
    flags = separateFlags strArgs
    parsedArgs = fst <$> foldr runFlag (Right (defaultArgs file, defaultArgsGiven)) flags


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
