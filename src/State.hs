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

module State ( State(..)
             , mkDefaultState
             , size
             , getSeed
             , nextGen
             , nextState
             , runProp
             , runShow
             , runCond
             , runLabels
             ) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import System.Random (randomIO, StdGen, newStdGen, genWord32, RandomGen)
import qualified System.Posix.DynamicLinker as DL
import qualified Data.Map.Strict as M

import qualified ParseFut as PF
import qualified FutInterface as FI
import FutInterface (CInt, CLong, Ptr, FutharkTestData, Futhark_Context, Stage(..))

-- State maintained during testing of a single test
data State = MkState
  { stateTestName             :: String
  , property                  :: CLong -> CInt -> ExceptT Stage IO Bool
  , condition                 :: Maybe (CLong -> CInt -> ExceptT Stage IO Bool)
  , shower                    :: Maybe (CLong -> CInt -> ExceptT Stage IO String)
  , labeler                   :: Maybe (CLong -> CInt -> ExceptT Stage IO String)
  , labels                    :: Maybe (M.Map String CInt)
  , numSuccessTests           :: CInt
  , maxSuccessTests           :: CInt
  , numDiscardedTests         :: CInt
  , numRecentlyDiscardedTests :: CInt
  , maxDiscardedRatio         :: CInt
  , maxSize                   :: CLong
  , computeSize               :: CLong -> CLong
  , randomSeed                :: StdGen
  }

instance Show State where
  show state = show (size state) ++ " " ++ show (getSeed state)

size :: State -> CLong
size state = (computeSize state (fromIntegral $ numSuccessTests state))

-- Should this be fst . nextState?
getSeed :: State -> CInt
getSeed = fst. nextState
-- getSeed = toEnum . (`mod` fromEnum (maxBound :: CInt)) . fst . next . randomSeed

nextGen :: State -> StdGen
nextGen = snd . genWord32 . randomSeed

nextState :: State -> (CInt, State)
nextState state = (cInt, newState)
  where
    (int,newGen) = genWord32 $ randomSeed state
    cInt         = (toEnum . fromEnum) int
    newState     = state {randomSeed = newGen}

runProp state =
  property state (size state) (getSeed state)

runShow :: State -> IO (Maybe (Either Stage String))
runShow state =
  case shower state of
    Nothing -> return Nothing
    Just s  -> Just <$> runExceptT (s (size state) (getSeed state))

runCond state =
  case condition state of
    Nothing -> return Nothing
    Just c  -> Just <$> runExceptT (c (size state) (getSeed state))

runLabels state =
  case labeler state of
    Nothing -> return Nothing
    Just l  -> Just <$> runExceptT (l (size state) (getSeed state))

mkDefaultState :: DL.DL -> Ptr Futhark_Context -> PF.FutFunNames -> IO State
mkDefaultState dl ctx testNames = do
  gen      <- newStdGen
  dynProp  <- FI.mkProperty  dl ctx Prop $ PF.propName testNames
  dynCond  <- if PF.condFound testNames
              then Just <$> FI.mkProperty dl ctx Cond (PF.condName testNames)
              else return Nothing
  dynShow  <- if PF.showFound testNames
              then Just <$> FI.mkShow dl ctx Show (PF.showName testNames)
              else return Nothing
  dynLabel <- if PF.labelFound testNames then
                Just <$> FI.mkShow dl ctx Label (PF.labelName testNames)
              else return Nothing
  (dynMST, dynMS, dynMDR) <-
    if PF.stateFound testNames
    then do
      state <- FI.getFutState dl ctx $ PF.stateName testNames
      mt    <- FI.futGetStateField     dl ctx state "maxtests"
      ms    <- return 1 -- <- FI.futGetStateFieldLong dl ctx state "maxsize"
      mdr   <- FI.futGetStateField     dl ctx state "maxdiscardedratio"
      return (mt,ms,mdr)
    else return (100, 100, 100) -- move defaults to fut ?
  return $ MkState
    { stateTestName             = PF.ffTestName testNames
    , property                  = dynProp
    , condition                 = dynCond
    , shower                    = dynShow
    , labeler                   = dynLabel
    , labels                    = const M.empty <$> dynLabel
    , numSuccessTests           = 0
    , maxSuccessTests           = dynMST
    , numDiscardedTests         = 0
    , numRecentlyDiscardedTests = 0
    , maxDiscardedRatio         = dynMDR
    , maxSize                   = dynMS
    , computeSize               =
        \n -> round $ toRational dynMS * (toRational n / toRational (dynMST - 1)) -- dynMST = 1 crashes
    , randomSeed                = gen
    }
