{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad ((<=<))
import System.Environment(getArgs)
--import qualified System.Command as C
--import qualified System.Process as P
import qualified System.Process.Typed as TP
import System.Exit (ExitCode(ExitSuccess), exitSuccess, exitFailure)
import System.Directory (createDirectory, doesDirectoryExist)
import qualified System.Posix.DynamicLinker as DL
import Codec.Binary.UTF8.String (decode)
import Data.ByteString (pack)
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.UTF8 as U
import Data.Int  (Int32, Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr,FunPtr,castFunPtrToPtr,nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Storable (Storable, peek)
import Data.List (unfoldr, foldl')
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT,throwE)
import Foreign.C.Types (CInt(CInt))
import Control.Monad.IO.Class(liftIO)
import Foreign.C.String (newCString)

uncurry3 f (a,b,c) = f a b c

data Futhark_Context_Config
data Futhark_Context

data Futhark_u8_1d
data FutharkTestData
type ValuesType =  Ptr Futhark_Context
                -> Ptr Futhark_u8_1d -- Old fut array
                -> Ptr Word8         -- New array
                -> IO CInt          -- Error info? Is this the right type?

type ArbitraryType =  Ptr Futhark_Context
                   -> Ptr (Ptr FutharkTestData)
                   -> CInt               -- size
                   -> CInt               -- seed
                   -> IO CInt

type PropertyType =  Ptr Futhark_Context
                  -> Ptr Bool
                  -> Ptr FutharkTestData
                  -> IO CInt

type ShowType =  Ptr Futhark_Context
              -> Ptr (Ptr Futhark_u8_1d)
              -> Ptr FutharkTestData
              -> IO CInt

foreign import ccall "dynamic"
  mkConfig :: FunPtr (IO (Ptr Futhark_Context_Config)) -> IO (Ptr Futhark_Context_Config)

newFutConfig :: DL.DL -> IO (Ptr Futhark_Context_Config)
newFutConfig dl = do
  funCfg <- DL.dlsym dl "futhark_context_config_new"
  cfg <- mkConfig funCfg
  return cfg

foreign import ccall "dynamic"
  mkConfigFree :: FunPtr (Ptr Futhark_Context_Config -> IO ()) -> Ptr Futhark_Context_Config -> IO ()
newFutFreeConfig :: DL.DL -> Ptr Futhark_Context_Config -> IO ()
newFutFreeConfig dl cfg = do
  f <- DL.dlsym dl "futhark_context_config_free"
  mkConfigFree f cfg

foreign import ccall "dynamic"
  mkNewFutContext :: FunPtr (Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)) -> Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)
newFutContext :: DL.DL -> Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)
newFutContext dl cfg = do
  ctx_fun <- DL.dlsym dl "futhark_context_new"
  mkNewFutContext ctx_fun cfg

foreign import ccall "dynamic"
  mkContextFree :: FunPtr (Ptr Futhark_Context -> IO ()) -> Ptr Futhark_Context -> IO ()
freeFutContext dl ctx = do
  f <- DL.dlsym dl "futhark_context_free"
  mkContextFree f ctx

---- Get dimensions of fut array
--foreign import ccall
--  oldfuthark_shape_u8_1d :: Ptr Futhark_Context
--                      -> Ptr Futhark_u8_1d       -- Array.
--                      -> IO (Ptr Int)            -- size

--oldfutShape :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO Int
--oldfutShape ctx futArr = do

foreign import ccall "dynamic"
  mkFutShape :: FunPtr (Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO (Ptr CInt))
             ->         Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO (Ptr CInt)

futShape :: DL.DL -> Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO CInt
futShape dl ctx futArr = do
  f <- DL.dlsym dl "futhark_shape_u8_1d"
  shapePtr <- mkFutShape f ctx futArr
  peek shapePtr

foreign import ccall "dynamic"
  mkFutValues :: FunPtr ValuesType -> ValuesType
mkValues :: DL.DL
         -> Ptr Futhark_Context
         -> FunPtr ValuesType
         -> Ptr Futhark_u8_1d
         -> ExceptT CInt IO String
mkValues dl ctx valuesPtr futArr = ExceptT $ do
  shape <- futShape dl ctx futArr
  eitherArr <- runExceptT $ haskifyArr (fromIntegral shape) (mkFutValues valuesPtr ctx) futArr
  case eitherArr of
    Right hsList -> do
      return $ Right $ decode hsList
    Left errorcode -> return $ Left errorcode

foreign import ccall "dynamic"
  mkFutArb :: FunPtr ArbitraryType -> ArbitraryType

mkArbitrary dl ctx name = do
  arbPtr <- DL.dlsym dl ("futhark_entry_" ++ name)
  return $ haskify2 (mkFutArb arbPtr) ctx

foreign import ccall "dynamic"
  mkFutProp :: FunPtr PropertyType -> PropertyType
mkProperty dl ctx name = do
  propPtr <- DL.dlsym dl ("futhark_entry_" ++ name)
  return $ haskify (mkFutProp propPtr) ctx


foreign import ccall "dynamic"
  mkFutShow :: FunPtr ShowType -> ShowType
mkShow dl ctx name = do
  showPtr   <- DL.dlsym dl ("futhark_entry_" ++ name)
  futValues <- DL.dlsym dl "futhark_values_u8_1d"
  return $ \input -> do
    u8arr <- haskify (mkFutShow showPtr) ctx input
    mkValues dl ctx futValues u8arr


haskify :: Storable out
        => (Ptr Futhark_Context -> Ptr out -> input -> IO CInt)
        -> Ptr Futhark_Context
        -> input
        -> ExceptT CInt IO out
haskify c_fun ctx input =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr input
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left exitcode)

haskify2 :: Storable out
        => (Ptr Futhark_Context -> Ptr out -> input1 -> input2 -> IO CInt)
        -> Ptr Futhark_Context
        -> input1
        -> input2
        -> ExceptT CInt IO out
haskify2 c_fun ctx input1 input2 =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr input1 input2
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left exitcode)

haskifyArr size c_fun input =
  ExceptT $ allocaArray (fromIntegral size) $ (\outPtr -> do
    exitcode <- c_fun input outPtr
    if exitcode == 0
    then (return . Right) =<< peekArray size outPtr
    else return $ Left exitcode)


---- New []u8
--foreign import ccall "futhark_new_i8_1d"
--  oldfutNewArru8 :: Ptr Futhark_Context
--              -> Ptr Word8              -- The old array
--              -> Ptr Int64              -- The size
--              -> IO (Ptr Futhark_u8_1d) -- The fut array
--
---- Move to C array
--foreign import ccall
--  oldfuthark_values_u8_1d :: Ptr Futhark_Context
--                       -> Ptr Futhark_u8_1d -- Old fut array
--                       -> Ptr Word8         -- New array
--                       -> IO CInt          -- Error info? Is this the right type?
--
--
--oldfutValues :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> ExceptT CInt IO String
--oldfutValues ctx futArr = ExceptT $ do
--  shape <- futShape ctx futArr
--  eitherArr <- runExceptT $ haskifyArr shape (oldfuthark_values_u8_1d ctx) futArr
--  case eitherArr of
--    Right hsList -> do
--      return $ Right $ decode hsList
--    Left errorcode -> return $ Left errorcode
--
--




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
  , arbitrary       :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , property        :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , shower          :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
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
  { futArb  :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , futProp :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , futShow :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  }



loadFutFuns dl ctx testNames = do
  dynArb  <- mkArbitrary dl ctx $ arbName testNames
  dynProp <- mkProperty  dl ctx $ propName testNames
  dynShow <- if showFound testNames
             then Just <$> mkShow dl ctx (showName testNames)
             else return Nothing
  return MkFuns { futArb  = dynArb
                , futProp = dynProp
                , futShow = dynShow
                }

mkDefaultState :: String -> StdGen -> FutFuns -> State
mkDefaultState testName gen fs =
  MkState
  { stateTestName   = testName
  , arbitrary       = futArb fs
  , property        = futProp fs
  , shower          = futShow fs
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
        Right result ->
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

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  ioPrep <- sequence $ map (testIOprep dl ctx) testNames
  let states = map (uncurry3 mkDefaultState) ioPrep
  sequence_ $ map ((putStrLn . result2str) <=< infResults) states
  freeFutContext dl ctx
  newFutFreeConfig dl cfg
  DL.dlclose dl
