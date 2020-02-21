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
import Foreign.Storable (peek)
import Data.List (unfoldr)
import System.Random (randomIO, getStdGen, next, RandomGen)


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




data Futhark_u8_1d

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

futValues :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO (String)
futValues ctx futArr = do
  shape <- futShape ctx futArr
  allocaArray shape (\cArr -> do
    futhark_values_u8_1d ctx futArr cArr
    hsList <- peekArray shape cArr
    return $ decode hsList
    )

-- Get dimensions of fut array
foreign import ccall 
  futhark_shape_u8_1d :: Ptr Futhark_Context
                      -> Ptr futhark_u8_1d       -- Array
                      -> IO (Ptr Int)            -- size

futShape :: Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO Int
futShape ctx futArr = do
  shapePtr <- futhark_shape_u8_1d ctx futArr
  peek shapePtr

-- Entry
foreign import ccall 
  futhark_entry_entrance :: Ptr Futhark_Context
                         -> Ptr Bool                -- succeeded?
                         -> Ptr (Ptr Futhark_u8_1d) -- string
                         -> Int32                   -- seed
                         -> IO (Int32)              -- Possibly error msg?

futEntry :: Ptr Futhark_Context -> Int32 -> IO (Result (Ptr Futhark_u8_1d))
futEntry ctx seed = result
  where 
    result =
      alloca $ (\boolPtr -> do
        str <- 
          alloca $ (\strPtr -> do
            futhark_entry_entrance ctx boolPtr strPtr seed
            peek strPtr)
        bool <- peek boolPtr
        return $ if bool then Success else Failure str)

data Result a = Success | Failure a

main :: IO ()
main = do
  cfg <- futNewConfig
  ctx <- futNewContext cfg

  gen <- randomIO

  futResult <- futEntry ctx gen
  case futResult of
    Success -> putStrLn "Success!"
    Failure futStr -> do
      str <- futValues ctx futStr
      putStrLn $ str
      

  futFreeContext ctx
  futFreeConfig cfg


--testLoop :: RandomGen g => Ptr Futhark_Context -> Int -> g -> IO ([Bool])
--testLoop ctx n gen = results
--  where
--    seeds = myIterate next32 gen
--    ios = map (futEntry ctx) seeds
--    results = sequence $ take n ios
  
next32 :: RandomGen g => g -> (Int32, g)
next32 g = (toEnum int, newGen)
  where (int,newGen) = next g

myIterate :: (a -> (b,a)) -> a -> [b]
myIterate f x = unfoldr (\x -> Just $ f x) x

