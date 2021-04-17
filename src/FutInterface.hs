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

module FutInterface ( Futhark_Context
                    , FutharkTestData
                    , FutState
                    , newFutConfig
                    , newFutFreeConfig
                    , newFutContext
                    , freeFutContext
                    , mkArbitrary
                    , mkProperty
                    , mkShow
                    , getFutState
                    , futGetStateField
                    , futGetStateFieldLong
                    , Ptr
                    , CInt(CInt)
                    , CLong(CLong)
                    , CBool(CBool)
                    , Stage(..)
                    , stage2str
                    ) where

import Foreign.Ptr (Ptr, FunPtr)--,castFunPtrToPtr,nullFunPtr)
import Data.Word (Word8)
import Foreign.C.Types (CInt(CInt),CLong(CLong), CBool(CBool))
import Control.Monad.Trans.Except(ExceptT(ExceptT), runExceptT,throwE)
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import qualified System.Posix.DynamicLinker as DL
import Codec.Binary.UTF8.String (decode)

(<.>) f g n = f <$> g n

-- For reporting where errors occur
data Stage =
    Arb   {exitCode :: CInt}
  | Prop  {exitCode :: CInt}
  | Cond  {exitCode :: CInt}
  | Show  {exitCode :: CInt}
  | Label {exitCode :: CInt}
  | GetState {exitCode :: CInt} deriving Show

stage2str (Arb _)   = "arbitrary"
stage2str (Prop _)  = "property"
stage2str (Cond _)  = "condition"
stage2str (Show _)  = "show"
stage2str (Label _) = "label"
stage2str (GetState _) = "state"

data Futhark_Context_Config
data Futhark_Context
data FutState

data Futhark_u8_1d
data FutharkTestData

-- Shorthands for various C function types
type ValuesType =  Ptr Futhark_Context
                -> Ptr Futhark_u8_1d -- Old fut array
                -> Ptr Word8         -- New array
                -> IO CInt          -- Error info? Is this the right type?

type ArbitraryType =  Ptr Futhark_Context
                   -> Ptr (Ptr FutharkTestData)
                   -> CLong               -- size
                   -> CInt               -- seed
                   -> IO CInt

type PropertyType =  Ptr Futhark_Context
                    -> Ptr CBool
                    -> CLong
                    -> CInt
                    -> IO CInt

type ShowType =  Ptr Futhark_Context
                -> Ptr (Ptr Futhark_u8_1d)
                -> CLong
                -> CInt
                -> IO CInt


type StateType = Ptr Futhark_Context
              -> Ptr (Ptr FutState)
              -> IO CInt

-- Create and free config and context
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

-- Get fucheck state
foreign import ccall "dynamic"
  mkFutState :: FunPtr StateType -> StateType
getFutState :: DL.DL -> Ptr Futhark_Context -> String -> IO (Ptr FutState)
getFutState dl ctx name = do
  futStatePtr <- DL.dlsym dl ("futhark_entry_entry_" ++ name)
  eitherFutState <- runExceptT $ haskify0 (mkFutState futStatePtr) ctx
  case eitherFutState of
    Right futState -> return futState
    Left exitCode -> error $ "Failed loading " ++ name ++ ".\nFailed with exit code " ++ show exitCode

-- get value of a field of state
-- make static ?
foreign import ccall "dynamic"
  mkGetStateField :: FunPtr (Ptr Futhark_Context -> Ptr CInt -> Ptr FutState -> IO CInt)
                  -> Ptr Futhark_Context -> Ptr CInt -> Ptr FutState -> IO CInt

foreign import ccall "dynamic"
  mkGetStateFieldLong :: FunPtr (Ptr Futhark_Context -> Ptr CLong -> Ptr FutState -> IO CInt)
                      -> Ptr Futhark_Context -> Ptr CLong -> Ptr FutState -> IO CInt

-- make static ?
futGetStateField :: DL.DL -> Ptr Futhark_Context -> Ptr FutState -> String -> IO CInt
futGetStateField dl ctx futState field = do
  fieldfun <- DL.dlsym dl ("futhark_entry_" ++ field)
  eitherfield <- runExceptT $ haskify (mkGetStateField fieldfun) ctx GetState futState
  case eitherfield of
    Right field -> return field
    Left errVal ->
      error $ "Failed getting " ++ field ++ " with exit code " ++ show (exitCode errVal)

futGetStateFieldLong :: DL.DL -> Ptr Futhark_Context -> Ptr FutState -> String -> IO CLong
futGetStateFieldLong dl ctx futState field = do
  fieldfun <- DL.dlsym dl ("futhark_entry_" ++ field)
  eitherfield <- runExceptT $ haskifyLong (mkGetStateFieldLong fieldfun) ctx GetState futState
  case eitherfield of
    Right field -> return field
    Left errVal ->
      error $ "Failed getting " ++ field ++ " with exit code " ++ show (exitCode errVal)


-- Helper functions for marshalling arrays
-- Get size of array
foreign import ccall "dynamic"
  mkFutShape :: FunPtr (Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO (Ptr CInt))
             ->         Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO (Ptr CInt)

futShape :: DL.DL -> Ptr Futhark_Context -> Ptr Futhark_u8_1d -> IO CInt
futShape dl ctx futArr = do
  f <- DL.dlsym dl "futhark_shape_u8_1d"
  shapePtr <- mkFutShape f ctx futArr
  peek shapePtr

-- Gets array as string
foreign import ccall "dynamic"
  mkFutValues :: FunPtr ValuesType -> ValuesType
mkValues :: DL.DL
         -> Ptr Futhark_Context
         -> (CInt -> Stage)
         -> FunPtr ValuesType
         -> Ptr Futhark_u8_1d
         -> ExceptT Stage IO String
mkValues dl ctx stage valuesPtr futArr = ExceptT $ do
  shape <- futShape dl ctx futArr
  eitherArr <- runExceptT $ haskifyArr (fromIntegral shape) (mkFutValues valuesPtr ctx) futArr
  case eitherArr of
    Right hsList -> do
      return $ Right $ decode hsList
    Left errorcode -> return $ Left $ stage errorcode

-- loads arbitrary function
foreign import ccall "dynamic"
  mkFutArb :: FunPtr ArbitraryType -> ArbitraryType

mkArbitrary dl ctx name = do
  arbPtr <- DL.dlsym dl ("futhark_entry_entry" ++ name)
  return $ haskify2 (mkFutArb arbPtr) ctx Arb

-- loads property and condition
foreign import ccall "dynamic"
  mkFutProp :: FunPtr PropertyType -> PropertyType
mkProperty dl ctx stage name = do
  propPtr <- DL.dlsym dl ("futhark_entry_entry_" ++ name)
  return $ \input0 input1 -> (0 /=) <$> (haskify2 (mkFutProp propPtr) ctx stage input0 input1)

-- Loads show and labels
foreign import ccall "dynamic"
  mkFutShow :: FunPtr ShowType -> ShowType
mkShow dl ctx stage name = do
  showPtr   <- DL.dlsym dl ("futhark_entry_entry_" ++ name)
  futValues <- DL.dlsym dl "futhark_values_u8_1d"
  return $ \input0 input1 -> do
    u8arr <- haskify2 (mkFutShow showPtr) ctx stage input0 input1
    mkValues dl ctx stage futValues u8arr

-- Helper functions
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
        -> (CInt -> Stage)
        -> input
        -> ExceptT Stage IO out
haskify c_fun ctx stage input =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr input
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left $ stage exitcode)

haskifyLong :: Storable out
            => (Ptr Futhark_Context -> Ptr out -> input -> IO CInt)
            -> Ptr Futhark_Context
            -> (CInt -> Stage)
            -> input
            -> ExceptT Stage IO out
haskifyLong c_fun ctx stage input =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr input
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left $ stage exitcode)

haskify2 :: Storable out
        => (Ptr Futhark_Context -> Ptr out -> input1 -> input2 -> IO CInt)
        -> Ptr Futhark_Context
        -> (CInt -> Stage)
        -> input1
        -> input2
        -> ExceptT Stage IO out
haskify2 c_fun ctx stage input1 input2 =
  ExceptT $ alloca $ (\outPtr -> do
    exitcode <- c_fun ctx outPtr input1 input2
    if exitcode == 0
    then (return . Right) =<< peek outPtr
    else return $ Left $ stage exitcode)

haskifyArr size c_fun input =
  ExceptT $ allocaArray (fromIntegral size) $ (\outPtr -> do
    exitcode <- c_fun input outPtr
    if exitcode == 0
    then (return . Right) =<< peekArray size outPtr
    else return $ Left exitcode)
