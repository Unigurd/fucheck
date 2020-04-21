module State ( State
             , mkDefaultState
             , stateTestName
             , arbitrary
             , property
             , shower
             , maxSuccessTests
             , numSuccessTests
             , size
             , getSeed
             , nextState
             ) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)

import FutInterface (FutharkTestData, CInt, Ptr)
import FutFuns (FutFuns(..))

data State = MkState
  { stateTestName   :: String
  , arbitrary       :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , property        :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , shower          :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  , numSuccessTests :: CInt
  , maxSuccessTests :: CInt
  , maxSize         :: CInt
  , computeSize     :: CInt -> CInt
  , randomSeed      :: StdGen
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

mkDefaultState :: String -> StdGen -> FutFuns -> State
mkDefaultState testName gen fs =
  MkState
  { stateTestName   = testName
  , arbitrary       = futArb fs
  , property        = futProp fs
  , shower          = futShow fs
  , numSuccessTests = 0
  , maxSuccessTests = futMaxSuccessTests fs
  , maxSize         = futMaxSize fs
  , computeSize     = \n -> round $ toRational (futMaxSize fs) * (toRational n / toRational (futMaxSuccessTests fs - 1))
  , randomSeed      = gen
  }
