module FutInterface ( FutharkTestData
                    , FutState
                    , newFutConfig
                    , newFutFreeConfig
                    , newFutContext
                    , freeFutContext
                    , mkArbitrary
                    , mkProperty
                    , mkShow
                    , getFutState
                    , futMaxTests
                    , Ptr
                    , CInt(CInt)
                    , CBool(CBool)) where

import Foreign.Ptr (Ptr, FunPtr)--,castFunPtrToPtr,nullFunPtr)
import Data.Word (Word8)
import Foreign.C.Types (CInt(CInt), CBool(CBool))
import Control.Monad.Trans.Except(ExceptT(ExceptT), runExceptT,throwE)
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import qualified System.Posix.DynamicLinker as DL
import Codec.Binary.UTF8.String (decode)

data Futhark_Context_Config
data Futhark_Context
data FutState

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
                  -> Ptr CBool
                  -> Ptr FutharkTestData
                  -> IO CInt

type ShowType =  Ptr Futhark_Context
              -> Ptr (Ptr Futhark_u8_1d)
              -> Ptr FutharkTestData
              -> IO CInt

type StateType = Ptr Futhark_Context
              -> Ptr (Ptr FutState)
              -> IO CInt

foreign import ccall "dynamic"
  mkConfig :: FunPtr (IO (Ptr Futhark_Context_Config)) -> IO (Ptr Futhark_Context_Config)

newFutConfig :: DL.DL -> IO (Ptr Futhark_Context_Config)
newFutConfig dl = do
  funCfg <- DL.dlsym dl "futhark_context_config_new"
  mkConfig funCfg

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
  mkFutState :: FunPtr StateType -> StateType
getFutState :: DL.DL -> Ptr Futhark_Context -> String -> IO (Ptr FutState)
getFutState dl ctx name = do
  futStatePtr <- DL.dlsym dl ("futhark_entry_" ++ name)
  eitherFutState <- runExceptT $ haskify0 (mkFutState futStatePtr) ctx
  case eitherFutState of
    Right futState -> return futState
    Left exitCode -> error $ "Failed loading " ++ name ++ ".\nFailed with exit code " ++ show exitCode

-- make static
foreign import ccall "dynamic"
  mkMaxTests :: FunPtr (Ptr Futhark_Context -> Ptr CInt -> Ptr FutState -> IO CInt)
                     -> Ptr Futhark_Context -> Ptr CInt -> Ptr FutState -> IO CInt

-- make static
futMaxTests dl ctx futState = do
  maxtestsfun <- DL.dlsym dl "futhark_entry_maxtests"
  eitherMaxTests <- runExceptT $ haskify (mkMaxTests maxtestsfun) ctx futState
  case eitherMaxTests of
    Right maxTests -> return maxTests
    Left exitCode -> error $ "Failed getting maxtests with exit code " ++ show exitCode

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
  return $ \input -> (0 /=) <$> (haskify (mkFutProp propPtr) ctx input)


foreign import ccall "dynamic"
  mkFutShow :: FunPtr ShowType -> ShowType
mkShow dl ctx name = do
  showPtr   <- DL.dlsym dl ("futhark_entry_" ++ name)
  futValues <- DL.dlsym dl "futhark_values_u8_1d"
  return $ \input -> do
    u8arr <- haskify (mkFutShow showPtr) ctx input
    mkValues dl ctx futValues u8arr

haskify0 :: Storable out
        => (Ptr Futhark_Context -> Ptr out -> IO CInt)
        -> Ptr Futhark_Context
        -> ExceptT CInt IO out
haskify0 c_fun ctx =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left exitcode)

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
