{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Binary.UTF8.String (decode)
import Data.ByteString (pack)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as U
import Data.Int  (Int32, Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Storable (Storable, peek)
import Data.List (unfoldr, foldl')
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)


data Futhark_Context_Config
foreign import ccall "futhark_context_config_new"
  futNewConfig:: IO (Ptr Futhark_Context_Config)

data Futhark_Context
foreign import ccall "futhark_context_new"
  futNewContext :: Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)

foreign import ccall "futhark_context_free"
  futFreeContext :: Ptr Futhark_Context -> IO ()

foreign import ccall "futhark_context_config_free"
  futFreeConfig :: Ptr Futhark_Context_Config -> IO ()

right (Right r) = r



type Cint = Int32
data Futhark_u8_1d
data FutharkTestData

haskify :: Storable out
        => (Ptr out -> input -> IO Cint)
        -> input
        -> ExceptT Cint IO out
haskify c_fun input =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun outPtr input
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left exitcode)

haskify2 :: Storable out
        => (Ptr out -> input1 -> input2 -> IO Cint)
        -> input1
        -> input2
        -> ExceptT Cint IO out
haskify2 c_fun input1 input2 =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun outPtr input1 input2
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
                       -> IO Cint          -- Error info? Is this the right type?


futValues :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> ExceptT Cint IO String
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

-- Arbitrary
foreign import ccall
  futhark_entry_arbitrary :: Ptr Futhark_Context
                          -> Ptr futharkTestData
                          -> Cint               -- size
                          -> Cint               -- seed
                          -> IO Cint

futArbitrary :: Ptr Futhark_Context -> Cint -> Cint -> ExceptT Cint IO (Ptr futharkTestData)
futArbitrary ctx = haskify2 (futhark_entry_arbitrary ctx)


-- Property
foreign import ccall
  futhark_entry_property :: Ptr Futhark_Context
                         -> Ptr Bool
                         -> Ptr FutharkTestData
                         -> IO Cint

futProperty :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT Cint IO Bool
futProperty ctx = haskify (futhark_entry_property ctx)

-- Show
foreign import ccall
  futhark_entry_show :: Ptr Futhark_Context
                     -> Ptr (Ptr Futhark_u8_1d)
                     -> Ptr FutharkTestData
                     -> IO Cint

futShow :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT Cint IO String
futShow ctx input = do
  u8arr <- haskify (futhark_entry_show ctx) input
  futValues ctx u8arr


spaces = ' ':spaces

indent n str =
  take n spaces ++ str

-- off by one?
padEndUntil end str = str ++ take (end - length str) spaces

formatMessages :: [(String, String)] -> [String]
formatMessages messages = lines
  where
    (names, values) = unzip messages
    longestName     = foldl' (\acc elm -> max acc $ length elm) 0 names
    formatName      = padEndUntil longestName . (++ ":")
    formattedNames  = map formatName names
    lines           = zipWith (++) formattedNames values

funCrash :: String -> [(String,String)] -> [String]
funCrash stage messages = crashMessage
  where
    restLines       = formatMessages messages
    crashMessage    = stage:(indent 2 <$> restLines)

crashMessage :: Cint -> [(String,[(String,String)])] -> [String]
crashMessage seed messages = crashMessage
  where
    crashLine = ("Futhark crashed on seed " ++ show seed)
                : [indent 2 "in function(s)"]
    lines = uncurry funCrash =<< messages
    crashMessage = crashLine ++ (indent 2 <$> lines)




data Stage = Arb | Test | Show
stage2str Arb  = "arbitrary"
stage2str Test = "property"
stage2str Show = "show"

data Result = Success
            | Failure (Either Cint String) Cint              -- input, seed
            | Exception (Maybe (Either Cint String)) Stage Cint Cint -- input, stage, error code, seed

data State = MkState
  {
    ctx             :: Ptr Futhark_Context
  , maxSuccessTests :: Int
  , computeSize     :: Int -> Cint
  , numSuccessTests :: Int
  , randomSeed      :: StdGen
  }

size :: State -> Cint
size state = (computeSize state (numSuccessTests state))

getSeed :: State -> Cint
getSeed = toEnum . fst . next . randomSeed

nextGen = snd . next . randomSeed

nextState :: State -> (Cint, State)
nextState state = (cInt, newState)
  where
    (int,newGen) = next $ randomSeed state
    cInt         = toEnum int
    newState     = state {randomSeed = newGen}

someFun :: State -> IO Result
someFun state = do
  let seed = getSeed state
  eTestdata <- runExceptT $ futArbitrary (ctx state) (size state) seed
  case eTestdata of
    Left arbExitCode -> return $ Exception Nothing Arb arbExitCode seed -- ARGH!
    Right testdata -> do
      eResult <- runExceptT $ futProperty (ctx state) testdata
      case eResult of
        Left propExitCode -> do
          eStr <- runExceptT $ futShow (ctx state) testdata
          case eStr of
            Right str -> return $ Exception (Just (Right str)) Test propExitCode seed
            Left showExitCode -> return $ Exception (Just (Left showExitCode)) Test propExitCode seed
        Right result ->
          if result
          then return Success
          else do
            eStrInput <- runExceptT $ futShow (ctx state) testdata
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
  "Test failed on input " ++ str
result2str (Failure (Left exitCode) seed) =
  unlines $ ("Test failed on seed " ++ show seed)
  : crashMessage seed [("show",[("Exit code", show exitCode)])]
result2str (Exception Nothing stage exitCode seed) =
  unlines $ crashMessage seed [((stage2str stage),[("Exit code", show exitCode)])]

result2str (Exception (Just (Right input)) stage exitCode seed) =
  unlines $ crashMessage seed [((stage2str stage), [ ("Exit code", show exitCode)
                                                   , ("Input", input)
                                                   ])
                              ]
result2str (Exception (Just (Left showExitCode)) stage exitCode seed) =
  unlines $ crashMessage seed [ ((stage2str stage),[("Exit code", show exitCode)])
                              , ("show", [("Exit code", show showExitCode)])
                              ]

main :: IO ()
main = do
  cfg <- futNewConfig
  ctx <- futNewContext cfg

  gen <- getStdGen
  let state = MkState
        { ctx             = ctx
        , maxSuccessTests = 100
        , computeSize     = toEnum . \n -> n `div` (maxSuccessTests state)
        , numSuccessTests = 0
        , randomSeed      = gen
        }


  result <- infResults state
  putStrLn $ result2str result

  futFreeContext ctx
  futFreeConfig cfg
