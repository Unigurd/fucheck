-- Copyright (C) Sigurd Dam Sonniks

-- This file is part of Fucheck.

--     Fucheck is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     Fucheck is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with Fucheck.  If not, see <https://www.gnu.org/licenses/>.

module CLI ( WhichTests(..)
           , Compiler(..)
           , Action(..)
           , Args(..)
           , parseArgs
           , filterTests
           , futArgs
           , gccArgs
           ) where

import Text.Read (readMaybe)
import ParseFut (only, without)

-- Which tests to include/exclude
data WhichTests = All | Only [String] | Without [String] | First Int | Last Int
filterTests All tests             = tests
filterTests (Only these) tests    = only these tests
filterTests (Without these) tests = without these tests
filterTests (First n) tests       = take n tests
filterTests (Last n) tests        = takeLast n tests
  where takeLast n = reverse . take n . reverse

-- Which compiler to use
data Compiler = C | OpenCL deriving Show

-- Arguments to futhark
futArgs args futFile tmpFile =
  case compiler args of
    C      -> ["c",      "--library", "-o", tmpFile, futFile]
    OpenCL -> ["opencl", "--library", "-o", tmpFile, futFile]

-- Arguments to gcc
gccArgs args tmpFile =
  case compiler args of
    C      -> [tmpFile ++ ".c",             "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
    OpenCL -> [tmpFile ++ ".c", "-lOpenCL", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]

-- Whether to save preprocessed program or test it
data Action = Run | PrintPreprocessed deriving Eq

-- CLI args
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

-- Keeps track of which arguments have already been read
data ArgsGiven = ArgsGiven { whichTestsGiven :: Bool
                           , compilerGiven   :: Bool
                           , actionGiven     :: Bool
                           }
defaultArgsGiven = ArgsGiven { whichTestsGiven = False
                             , compilerGiven   = False
                             , actionGiven     = False
                             }




getCompiler ("opencl":rest) = (OpenCL, rest)
getCompiler ("c":rest)      = (C, rest)
getCompiler rest            = (C, rest)

getFilename [] = errorWithoutStackTrace "Missing file argument"
getFilename (filename:rest) = (filename, rest)

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


-- The different flags
compilerFlags   = ["c","--c","opencl","--opencl"]
testFilterFlags = ["--only","--without","--first","--last"]
actionFlags     = ["--out", "-o", "--run"]

-- Test whether a word matches some type of flag
matches :: String -> [String] -> Bool
flag `matches` flagList = or $ (flag ==) <$> flagList

-- converts words to various flags
compFlag2comp "c"        = Right C
compFlag2comp "--c"      = Right C
compFlag2comp "opencl"   = Right OpenCL
compFlag2comp "--opencl" = Right OpenCL
compFlag2comp badFlag = Left $ "unrecognized compiler flag: " ++ badFlag

testFilterFlag2testFilter ("--only":args)    = Right $ Only args
testFilterFlag2testFilter ("--without":args) = Right $ Without args
testFilterFlag2testFilter ["--first"] = Right $ First 1
testFilterFlag2testFilter ["--first",numStr] =
  case readMaybe numStr of
    Just num -> if num >= 0 then
                  Right $ First num
                else Left
                     $ "--first " ++ numStr ++ ": "
                     ++ numStr ++ " is negative"
    Nothing -> Left
               $ "--last " ++ numStr ++ ": "
               ++ numStr ++ " not a number"
testFilterFlag2testFilter ["--last"] = Right $ Last 1
testFilterFlag2testFilter ["--last",numStr] =
  case readMaybe numStr of
    Just num -> if num >= 0 then
                  Right $ Last num
                else Left
                     $ "--last " ++ numStr ++ ": "
                     ++ numStr ++ " is negative"
testFilterFlag2testFilter _ = Left stdErrMsg

actionFlag2action ["--out"]      = Right PrintPreprocessed
actionFlag2action ["-o"]         = Right PrintPreprocessed
actionFlag2action ["--run"]      = Right Run
actionFlag2action _ = Left stdErrMsg

-- parses --c, --opencl, c and opencl
parseCompilerFlag [] _ = Left stdErrMsg
parseCompilerFlag [flag] (args,given) =
  if compilerGiven given then
    Left "Compiler was specified more than once"
  else do
    comp <- compFlag2comp flag
    Right $ (args { compiler = comp }, given { compilerGiven = True })
parseCompilerFlag (flag:_) _ = Left $ "Compiler arg " ++ flag ++ " does not take arguments"

-- parses --only and --without
parseTestFilter [] _ = Left stdErrMsg
parseTestFilter flag (args,given) =
  if whichTestsGiven given then
    Left "Test filtering was specified more than once"
  else do
    testFilter <- testFilterFlag2testFilter flag
    Right $ (args { whichTests = testFilter }, given { whichTestsGiven = True })

-- parses --save and --run
parseAction [] _ = Left stdErrMsg
parseAction flag (args,given) =
  if actionGiven given then
    Left "Action was specified more than once"
  else do
    action <- actionFlag2action flag
    Right $ (args { action = action }, given { actionGiven = True })

-- parses a flag
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

-- parses cli args
parseArgs :: [String] -> Either String Args
parseArgs [] = Left "No test file specified"
parseArgs allStrArgs = parsedArgs
  where
    file    = last allStrArgs
    strArgs = init allStrArgs
    flags = separateFlags strArgs
    parsedArgs = fst <$> foldr runFlag (Right (defaultArgs file, defaultArgsGiven)) flags
