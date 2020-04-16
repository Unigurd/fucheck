{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad ((<=<))
import System.Environment(getArgs)
import qualified System.Process.Typed as TP
import System.Exit (ExitCode(ExitSuccess), exitSuccess, exitFailure)
import System.Directory (createDirectory, doesDirectoryExist)
import qualified System.Posix.DynamicLinker as DL
import Data.ByteString (pack)
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.UTF8 as U
import Data.Int  (Int32, Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Data.List (unfoldr, foldl')
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import Foreign.C.Types (CInt(CInt))
import Control.Monad.IO.Class(liftIO)
import Foreign.C.String (newCString)

import qualified FutInterface as FI

uncurry3 f (a,b,c) = f a b c






spaces = ' ':spaces

indent n str =
  take n spaces ++ str

-- off by one?
padEndUntil end str = str ++ take (end - length str) spaces

formatMessages :: [(String, String)] -> [String]
formatMessages messages = lines
  where
    (names, values) = unzip messages
    namesColon      = (++ ": ") <$> names
    longestName     = foldl' (\acc elm -> max acc $ length elm) 0 namesColon
    formatName      = padEndUntil longestName
    formattedNames  = map formatName namesColon
    lines           = zipWith (++) formattedNames values

funCrash :: String -> [(String,String)] -> [String]
funCrash stage messages = crashMessage
  where
    restLines       = formatMessages messages
    crashMessage    = stage:(indent 2 <$> restLines)

crashMessage :: String -> CInt -> [(String,[(String,String)])] -> [String]
crashMessage name seed messages = crashMessage
  where
    crashLine = ("Property " ++ name ++ " crashed on seed " ++ show seed)
    lines                = uncurry funCrash =<< messages
    linesWithDescription = "in function(s)":(indent 2 <$> lines)
    crashMessage         = crashLine:(indent 2 <$> linesWithDescription)




data Stage = Arb | Test | Show
stage2str Arb  = "arbitrary"
stage2str Test = "property"
stage2str Show = "show"

data Result =
    Success
    { resultTestName :: String
    , numTests       :: Integer
    }
  | Failure
    -- Nothing if no attempt at showing could be made
    -- Just Left if it tried to generate a string but failed
    -- Just Right if a string was successfully generated
    { resultTestName :: String
    , shownInput     :: Maybe (Either CInt String)
    , resultSeed     :: CInt
    }
  | Exception
    { resultTestName :: String
    , shownInput     :: Maybe (Either CInt String)
    , errorStatge    :: Stage
    , futExitCode    :: CInt
    , resultSeed     :: CInt
    }

data State = MkState
  { stateTestName   :: String
  , arbitrary       :: CInt -> CInt -> ExceptT CInt IO (Ptr FI.FutharkTestData)
  , property        :: Ptr FI.FutharkTestData -> ExceptT CInt IO Bool
  , propTest        :: Ptr FI.FutharkTestData -> ExceptT CInt IO CInt
  , shower          :: Maybe (Ptr FI.FutharkTestData -> ExceptT CInt IO String)
  , maxSuccessTests :: Integer
  , numSuccessTests :: Integer
  , computeSize     :: Int -> CInt
  , randomSeed      :: StdGen
  }

data FutFunNames = FutFunNames
  { ffTestName :: String
  , arbName    :: String
  , propName   :: String
  , showName   :: String
  , arbFound   :: Bool
  , propFound  :: Bool
  , showFound  :: Bool
  }

newFutFunNames name = FutFunNames
  { ffTestName = name
  , arbName    = name ++ "arbitrary"
  , propName   = name ++ "property"
  , showName   = name ++ "show"
  , arbFound   = False
  , propFound  = False
  , showFound  = False
  }

data FutFuns = MkFuns
  { futArb  :: CInt -> CInt -> ExceptT CInt IO (Ptr FI.FutharkTestData)
  , futProp :: Ptr FI.FutharkTestData -> ExceptT CInt IO Bool
  , futShow :: Maybe (Ptr FI.FutharkTestData -> ExceptT CInt IO String)
  , futPropTest :: Ptr FI.FutharkTestData -> ExceptT CInt IO CInt
  }



loadFutFuns dl ctx testNames = do
  dynArb  <- FI.mkArbitrary dl ctx $ arbName testNames
  dynProp <- FI.mkProperty  dl ctx $ propName testNames
  dynShow <- if showFound testNames
             then Just <$> FI.mkShow dl ctx (showName testNames)
             else return Nothing
  dynPropTest <- FI.mkPT  dl ctx $ propName testNames
  return MkFuns { futArb  = dynArb
                , futProp = dynProp
                , futShow = dynShow
                , futPropTest = dynPropTest
                }

mkDefaultState :: String -> StdGen -> FutFuns -> State
mkDefaultState testName gen fs =
  MkState
  { stateTestName   = testName
  , arbitrary       = futArb fs
  , property        = futProp fs
  , shower          = futShow fs
  , propTest    = futPropTest fs
  , maxSuccessTests = 100
  , computeSize     = toEnum . \n ->  n
    -- (maxSuccessTests state) - (maxSuccessTests state) `div` (n+1)
  , numSuccessTests = 0
  , randomSeed      = gen
  }

size :: State -> CInt
size state = (computeSize state (fromIntegral $ numSuccessTests state))

getSeed :: State -> CInt
getSeed = toEnum . fst . next . randomSeed

nextGen = snd . next . randomSeed

nextState :: State -> (CInt, State)
nextState state = (cInt, newState)
  where
    (int,newGen) = next $ randomSeed state
    cInt         = toEnum int
    newState     = state {randomSeed = newGen}

f *< a = f <*> pure a

someFun :: State -> IO Result
someFun state = do
  let seed = getSeed state
  eTestdata <- runExceptT $ (arbitrary state) (size state) seed
  case eTestdata of
    Left arbExitCode -> return $ Exception (stateTestName state) Nothing Arb arbExitCode seed
    Right testdata -> do
      eResult <- runExceptT $ (property state) testdata
      case eResult of
        Left propExitCode -> do
          shownInput <- sequence $ runExceptT <$> shower state *< testdata
          return $ Exception (stateTestName state) shownInput Test propExitCode seed
        Right result -> do
          if result
            then return Success {resultTestName = stateTestName state, numTests = numSuccessTests state}
            else do
            shownInput2 <- sequence $ runExceptT <$> shower state *< testdata
            return $ Failure { resultTestName = stateTestName state
                             , shownInput     = shownInput2
                             , resultSeed     = seed
                             }

infResults :: State -> IO Result
infResults state
  | numSuccessTests state >= maxSuccessTests state = return $ Success
                                                     { resultTestName = stateTestName state
                                                     , numTests       = numSuccessTests state
                                                     }
  | otherwise = do
  result <- someFun state
  case result of
    Success _ _ -> infResults $ state { numSuccessTests = numSuccessTests state + 1
                                      , randomSeed      = nextGen state
                                      }
    _       -> return result

result2str :: Result -> String
result2str (Success name numTests) = "Property " ++ name ++ " holds after " ++ show numTests ++ " tests"
result2str (Failure name Nothing seed) =
  "Property " ++ name ++ " failed on seed " ++ show seed
result2str (Failure name (Just (Right str)) _) =
  "Property " ++ name ++ " failed on input " ++ str
result2str (Failure name (Just (Left exitCode)) seed) =
  unlines $ ("Property " ++ name ++ " failed on seed " ++ show seed)
  : crashMessage name seed [("show",[("Exit code", show exitCode)])]
result2str (Exception name Nothing stage exitCode seed) =
  unlines $ crashMessage name seed [((stage2str stage),[("Exit code", show exitCode)])]
result2str (Exception name (Just (Right input)) stage exitCode seed) =
  unlines $ crashMessage name seed [((stage2str stage), [ ("Input", input)
                                                        , ("Exit code", show exitCode)
                                                        ])]

result2str (Exception name (Just (Left showExitCode)) stage exitCode seed) =
  unlines $ crashMessage name seed [ ((stage2str stage),[("Exit code", show exitCode)])
                                   , ("show", [("Exit code", show showExitCode)])
                                   ]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr (\elm acc -> case f elm of
                          Just x  -> x:acc
                          Nothing -> acc) []

getTestName ["--", "fucheck", name] = Just name
getTestName _                       = Nothing

mapPerhaps :: (a -> Maybe a) -> [a] -> [a]
mapPerhaps f l = foldr (\elm acc -> case f elm of ; Nothing -> elm:acc ; Just newElm -> newElm:acc) [] l

funNameMatches ("entry":actualName:_) expectedName = actualName == expectedName
funNameMatches _ _ = False

anyFunNameMatches line ffns =
  if matchesLine $ arbName ffns
  then Just $ ffns {arbFound = True}
  else if matchesLine $ propName ffns
       then Just $ ffns {propFound = True}
       else if matchesLine $ showName ffns
            then Just $ ffns {showFound = True}
            else Nothing
  where matchesLine = funNameMatches line



checkLine foundFuns line =
  case getTestName line of
    Just newName -> newFutFunNames newName : foundFuns
    Nothing      -> mapPerhaps (anyFunNameMatches line) foundFuns

findTests :: String -> [FutFunNames]
findTests source = tests
  where
    tokens = words <$> lines source
    -- breaks if using foldr to preserve test order
    tests  = reverse $ foldl' checkLine [] tokens
      --filterMap getTestName tokens


--myReadProcess :: TP.ProcessConfig stdin stdoutIgnored stderrIgnored ->  ExceptT ExitCode IO (ByteString, ByteString)
--myReadProcess p = do
--  (exitCode, out, err) :: (ExitCode, ByteString, ByteString) <- return $ TP.readProcess p
--  case exitCode of
--    ExitSuccess -> return (out,err)
--    _           -> throwE exitCode

letThereBeDir dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then createDirectory dir
    else return ()

headWithDefault def []     = def
headWithDefault _ (head:_) = head

right (Right a) = a

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

  cfg <- FI.newFutConfig dl
  ctx <- FI.newFutContext dl cfg

  ioPrep <- sequence $ map (testIOprep dl ctx) testNames
  let states = map (uncurry3 mkDefaultState) ioPrep
  sequence_ $ map ((putStrLn . result2str) <=< infResults) states
  FI.freeFutContext dl ctx
  FI.newFutFreeConfig dl cfg
  DL.dlclose dl
