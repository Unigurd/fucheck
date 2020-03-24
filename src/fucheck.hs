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
import Data.List (unfoldr)
import System.Random (randomIO, getStdGen, next, RandomGen)
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
        => (Ptr out -> input -> IO Int32)
        -> input
        -> ExceptT Int32 IO out
haskify c_fun input =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun outPtr input
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
                       -> IO Int32          -- Error info? Is this the right type?


futValues :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> ExceptT Int32 IO String
futValues ctx futArr = ExceptT $ do
  shape <- futShape ctx futArr
  eitherArr <- runExceptT $ haskifyArr shape (futhark_values_u8_1d ctx) futArr 
  case eitherArr of
    Right hsList -> do
      return $ Right $ decode hsList
    Left errorcode -> return $ Left errorcode



--  allocaArray shape (\cArr -> do
--    errorcode <- futhark_values_u8_1d ctx futArr cArr
--    return $ 
--      if errorcode == 0
--      then Right $ do
--        hsList <- peekArray shape cArr
--        return $ decode hsList
--      else Left errorcode
--    )


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
                          -> Int32
                          -> IO Int32

futArbitrary :: Ptr Futhark_Context -> Int32 -> ExceptT Int32 IO (Ptr futharkTestData)
futArbitrary ctx = haskify (futhark_entry_arbitrary ctx)


-- Property
foreign import ccall 
  futhark_entry_property :: Ptr Futhark_Context
                         -> Ptr Bool
                         -> Ptr FutharkTestData
                         -> IO Int32

futProperty :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT Int32 IO Bool
futProperty ctx = haskify (futhark_entry_property ctx)

-- Show
foreign import ccall 
  futhark_entry_show :: Ptr Futhark_Context
                     -> Ptr (Ptr Futhark_u8_1d)
                     -> Ptr FutharkTestData
                     -> IO Int32


-- Use monad transformer?
futShow :: Ptr Futhark_Context -> Ptr FutharkTestData -> ExceptT Int32 IO String
futShow ctx input = do
  u8arr <- haskify (futhark_entry_show ctx) input
  futValues ctx u8arr

--  eitheru8arr <- haskify futhark_entry_show ctx input
--  ExceptT $ case eitheru8arr of
--    Right u8arr -> do
--      str <- futValues ctx u8arr
--      return $ Right str
--    Left errorcode -> return $ Left errorcode


-- Entry
foreign import ccall 
  futhark_entry_main :: Ptr Futhark_Context
                     -> Ptr Bool                -- succeeded?
                     -> Ptr (Ptr Futhark_u8_1d) -- string
                     -> Int32                   -- seed
                     -> IO (Int32)              -- Possibly error msg?


futEntry :: Ptr Futhark_Context -> Int32 -> IO Result
futEntry ctx seed = do
  alloca $ (\boolPtr -> do
    entryResult <- 
      alloca $ (\strPtr -> do
        exitCode <- futhark_entry_main ctx boolPtr strPtr seed
        if exitCode == 0
        then (return . Right) =<< peek strPtr
        else return $ Left exitCode
        )

    case entryResult of
      Left exitCode -> return $ Exception exitCode seed
      Right str -> do
        bool <- peek boolPtr
        return $ if bool then Success else Failure str seed)

data Result = Success
            | Failure (Ptr (Futhark_u8_1d)) Int32 -- Error message and seed
            | Exception Int32 Int32               -- Error exitCode and seed

main :: IO ()
main = do
  cfg <- futNewConfig
  ctx <- futNewContext cfg

  gen <- getStdGen

  let exceptBool = futProperty ctx =<< (futArbitrary ctx $ toEnum $ fst $ next gen)

  exceptResult <- runExceptT exceptBool
  putStrLn $ case exceptResult of
    Right result   ->  (if result then "Yay! " else "Test failed >:( " ) ++ show result
    Left errorcode -> "CRASH! " ++ show errorcode
     

  let tests = testLoop ctx gen
  result <- doTests 100 tests

  case result of
    Success -> putStrLn "Success"
    Failure futStr seed -> do
      str <- runExceptT $ futValues ctx futStr
      putStrLn $ "Failure with input " ++ (right str) ++ " from seed " ++ show seed
    Exception exitCode seed -> putStrLn ("Futhark crashed with exit code " ++ show exitCode ++ " from seed " ++ show seed)

  futFreeContext ctx
  futFreeConfig cfg

coalesce Success Success = Success
coalesce Success failure = failure
coalesce failure _       = failure

doTests :: Int -> [IO Result] -> IO Result
doTests n ios = do
  results <- sequence $ take n ios
  return $ foldl coalesce Success results

testLoop :: RandomGen g => Ptr Futhark_Context -> g -> [IO Result]
testLoop ctx gen = results
  where
    seeds      = myIterate next32 gen
    results = map (futEntry ctx) seeds

next32 :: RandomGen g => g -> (Int32, g)
next32 g = (toEnum int, newGen)
  where (int,newGen) = next g

myIterate :: (a -> (b,a)) -> a -> [b]
myIterate f x = unfoldr (\x -> Just $ f x) x
