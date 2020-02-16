{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Int
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
--import System.Random
                       

data Futhark_Context_Config
foreign import ccall "futhark_context_config_new"
  futhark_context_config_new :: IO (Ptr Futhark_Context_Config)

data Futhark_Context
foreign import ccall "futhark_context_new"
  futhark_context_new :: Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)

foreign import ccall "futhark_entry_test_entry"
  futhark_entry_test_entry :: Ptr Futhark_Context -> Ptr Bool
                           -> Int32 -> IO ()

foreign import ccall "futhark_context_free"
  futhark_context_free :: Ptr Futhark_Context -> IO ()

foreign import ccall "futhark_context_config_free"
  futhark_context_config_free :: Ptr Futhark_Context_Config -> IO ()


main :: IO ()
main = do
  cfg <- futhark_context_config_new
  ctx <- futhark_context_new cfg

  res <- alloca $ (\res -> do 
    futhark_entry_test_entry ctx res 32
    peek res)

  putStrLn $ "Result " ++ show res

  futhark_context_free ctx
  futhark_context_config_free cfg
