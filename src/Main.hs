{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment(getArgs)
--import qualified System.Command as C
--import qualified System.Process as P
import qualified System.Process.Typed as TP
import System.Exit (exitSuccess)
import qualified System.Posix.DynamicLinker as DL
import Codec.Binary.UTF8.String (decode)
import Data.ByteString (pack)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as U
import Data.Int  (Int32, Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr,FunPtr,castFunPtrToPtr,nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Storable (Storable, peek)
import Data.List (unfoldr, foldl')
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import Foreign.C.Types (CInt(CInt))
import Control.Monad.IO.Class(liftIO)
import Foreign.C.String (newCString)

data Futhark_Context_Config
foreign import ccall "futhark_context_config_new"
  oldFutNewConfig:: IO (Ptr Futhark_Context_Config)

foreign import ccall "futhark_context_config_set_debugging"
  oldSetDebug :: Ptr Futhark_Context_Config -> CInt -> IO ()

data Futhark_Context
foreign import ccall "futhark_context_new"
  oldFutNewContext :: Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)

foreign import ccall "futhark_context_free"
  oldFutFreeContext :: Ptr Futhark_Context -> IO ()

foreign import ccall "futhark_context_config_free"
  oldFutFreeConfig :: Ptr Futhark_Context_Config -> IO ()

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
  mkSetDebug :: FunPtr (Ptr Futhark_Context_Config -> CInt -> IO ())
             ->         Ptr Futhark_Context_Config -> CInt -> IO () -- Svarer IO () til void?

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

foreign import ccall "dynamic"
  mkFutValues :: FunPtr ValuesType -> ValuesType
mkValues :: Ptr Futhark_Context -> FunPtr ValuesType -> Ptr Futhark_u8_1d -> ExceptT CInt IO String
mkValues ctx valuesPtr futArr = ExceptT $ do
  shape <- futShape ctx futArr
  eitherArr <- runExceptT $ haskifyArr shape (mkFutValues valuesPtr ctx) futArr
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
    mkValues ctx futValues u8arr


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
  ExceptT $ allocaArray size $ (\outPtr -> do
    exitcode <- c_fun input outPtr
    if exitcode == 0
    then (return . Right) =<< peekArray size outPtr
    else return $ Left exitcode)


-- New []u8
foreign import ccall "futhark_new_i8_1d"
  futNewArru8 :: Ptr Futhark_Context
              -> Ptr Word8              -- The old array
              -> Ptr Int64              -- The size
              -> IO (Ptr Futhark_u8_1d) -- The fut array

-- Move to C array
foreign import ccall
  futhark_values_u8_1d :: Ptr Futhark_Context
                       -> Ptr Futhark_u8_1d -- Old fut array
                       -> Ptr Word8         -- New array
                       -> IO CInt          -- Error info? Is this the right type?


futValues :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> ExceptT CInt IO String
futValues ctx futArr = ExceptT $ do
  shape <- futShape ctx futArr
  eitherArr <- runExceptT $ haskifyArr shape (futhark_values_u8_1d ctx) futArr
  case eitherArr of
    Right hsList -> do
      return $ Right $ decode hsList
    Left errorcode -> return $ Left errorcode


-- Get dimensions of fut array
foreign import ccall
  futhark_shape_u8_1d :: Ptr Futhark_Context
                      -> Ptr Futhark_u8_1d       -- Array.
                      -> IO (Ptr Int)            -- size

futShape :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO Int
futShape ctx futArr = do
  shapePtr <- futhark_shape_u8_1d ctx futArr
  peek shapePtr



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

crashMessage :: CInt -> [(String,[(String,String)])] -> [String]
crashMessage seed messages = crashMessage
  where
    crashLine = ("Futhark crashed on seed " ++ show seed)
    lines                = uncurry funCrash =<< messages
    linesWithDescription = "in function(s)":(indent 2 <$> lines)
    crashMessage         = crashLine:(indent 2 <$> linesWithDescription)




data Stage = Arb | Test | Show
stage2str Arb  = "arbitrary"
stage2str Test = "property"
stage2str Show = "show"

data Result = Success
            | Failure (Either CInt String) CInt              -- input, seed
            | Exception (Maybe (Either CInt String)) Stage CInt CInt -- input, stage, error code, seed

-- futArbitrary :: Ptr Futhark_Context -> CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
-- futProperty :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT CInt IO Bool
-- futShow :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT CInt IO String
data State = MkState
  {
    ctx             :: Ptr Futhark_Context
  , arbitrary       :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , property        :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , shower          :: Ptr FutharkTestData -> ExceptT CInt IO String
  , maxSuccessTests :: Int
  , computeSize     :: Int -> CInt
  , numSuccessTests :: Int
  , randomSeed      :: StdGen
  }

size :: State -> CInt
size state = (computeSize state (numSuccessTests state))

getSeed :: State -> CInt
getSeed = toEnum . fst . next . randomSeed

nextGen = snd . next . randomSeed

nextState :: State -> (CInt, State)
nextState state = (cInt, newState)
  where
    (int,newGen) = next $ randomSeed state
    cInt         = toEnum int
    newState     = state {randomSeed = newGen}

someFun :: State -> IO Result
someFun state = do
  let seed = getSeed state
  eTestdata <- runExceptT $ (arbitrary state) (size state) seed
  case eTestdata of
    Left arbExitCode -> return $ Exception Nothing Arb arbExitCode seed
    Right testdata -> do
      eResult <- runExceptT $ (property state) testdata
      case eResult of
        Left propExitCode -> do
          eStr <- runExceptT $ (shower state) testdata
          case eStr of
            Right str -> return $ Exception (Just (Right str)) Test propExitCode seed
            Left showExitCode -> return $ Exception (Just (Left showExitCode)) Test propExitCode seed
        Right result ->
          if result
          then return Success
          else do
            eStrInput <- runExceptT $ (shower state) testdata
            return $ Failure eStrInput seed

infResults :: State -> IO Result
infResults state
  | numSuccessTests state >= maxSuccessTests state = return Success
  | otherwise = do
  result <- someFun state
  case result of
    Success -> infResults $ state { numSuccessTests = numSuccessTests state + 1
                                  , randomSeed      = nextGen state
                                  }
    _       -> return result

result2str :: Result -> String
result2str Success = "Success!"
result2str (Failure (Right str) seed) =
  "Test failed on input " ++ iStr
  where
    iStr = unlines $ indent 2 <$> lines str
result2str (Failure (Left exitCode) seed) =
  unlines $ ("Test failed on seed " ++ show seed)
  : crashMessage seed [("show",[("Exit code", show exitCode)])]
result2str (Exception Nothing stage exitCode seed) =
  unlines $ crashMessage seed [((stage2str stage),[("Exit code", show exitCode)])]

result2str (Exception (Just (Right input)) stage exitCode seed) =
  unlines $ crashMessage seed [((stage2str stage), [ ("Input", input)
                                                   , ("Exit code", show exitCode)
                                                   ])]

result2str (Exception (Just (Left showExitCode)) stage exitCode seed) =
  unlines $ crashMessage seed [ ((stage2str stage),[("Exit code", show exitCode)])
                              , ("show", [("Exit code", show showExitCode)])
                              ]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr (\elm acc -> case f elm of
                          Just x  -> x:acc
                          Nothing -> acc) []

getTestName ["--", "fucheck", name] = Just name
getTestName _                       = Nothing

findTests :: String -> [String]
findTests source = tests
  where
    tokens = words <$> lines source
    tests  = filterMap getTestName tokens


headWithDefault def [] = def
headWithDefault _ (head:_) = head

right (Right a) = a

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

  dirExists <- doesDirectoryExist tmpDir
  if not dirExists
    then createDirectory tmpDir
    else return ()

  (futExitCode, futOut, futErr) <-
    TP.readProcess $ TP.proc "futhark" ["c", "--library", "-o", tmpFile, filename ++ ".fut"]
  putStrLn $ show futExitCode
  putStrLn $ show futOut
  putStrLn $ show futErr

  (gccExitCode, gccOut, gccErr) <-
    TP.readProcess $ TP.proc "gcc" [tmpFile ++ ".c", "-o", tmpFile ++ ".so", "-fPIC", "-shared"]
  putStrLn $ show gccExitCode
  putStrLn $ show gccOut
  putStrLn $ show gccErr


  dl <- DL.dlopen (tmpFile ++ ".so") [DL.RTLD_NOW] -- Read up on flags

  fileText <- readFile $ filename ++ ".fut"
  let testNames = findTests fileText
  let firstTest = head testNames

  cfg <- newFutConfig dl
  ctx <- newFutContext dl cfg

  dynArb  <- mkArbitrary dl ctx $ firstTest ++ "arbitrary"
  dynProp <- mkProperty  dl ctx $ firstTest ++ "property"
  dynShow <- mkShow      dl ctx $ firstTest ++ "show"




  gen <- getStdGen
  let state = MkState
        { ctx             = ctx
        , arbitrary       = dynArb
        , property        = dynProp
        , shower          = dynShow
        , maxSuccessTests = 100
        , computeSize     = toEnum . \n ->  n
          -- (maxSuccessTests state) - (maxSuccessTests state) `div` (n+1)
        , numSuccessTests = 0
        , randomSeed      = gen
        }

  result <- infResults state
  putStrLn $ result2str result

  --oldFutFreeContext ctx
  freeFutContext dl ctx
  --oldFutFreeConfig cfg
  newFutFreeConfig dl cfg
  DL.dlclose dl
