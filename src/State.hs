module State ( State(..)
             , mkDefaultState
             , size
             , getSeed
             , nextGen
             , nextState
             ) where

import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import qualified System.Posix.DynamicLinker as DL
import qualified Data.Map.Strict as M

import qualified ParseFut as PF
import qualified FutInterface as FI
import FutInterface (CInt, Ptr, FutharkTestData, Futhark_Context)

data State = MkState
  { stateTestName             :: String
  , arbitrary                 :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , property                  :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , condition                 :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO Bool)
  , shower                    :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  , labeler                   :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  , labels                    :: Maybe (M.Map String CInt)
  , numSuccessTests           :: CInt
  , maxSuccessTests           :: CInt
  , numDiscardedTests         :: CInt
  , numRecentlyDiscardedTests :: CInt
  , maxDiscardedRatio         :: CInt
  , maxSize                   :: CInt
  , computeSize               :: CInt -> CInt
  , randomSeed                :: StdGen
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


mkDefaultState :: DL.DL -> Ptr Futhark_Context -> PF.FutFunNames -> IO State
mkDefaultState dl ctx testNames = do
  gen <- getStdGen
  dynArb  <- FI.mkArbitrary dl ctx $ PF.arbName testNames
  dynProp <- FI.mkProperty  dl ctx $ PF.propName testNames
  dynCond <- if PF.condFound testNames
             then Just <$> FI.mkProperty dl ctx (PF.condName testNames)
             else return Nothing
  dynShow <- if PF.showFound testNames
             then Just <$> FI.mkShow dl ctx (PF.showName testNames)
             else return Nothing
  dynLabel <- if PF.labelFound testNames then
                Just <$> FI.mkShow dl ctx (PF.labelName testNames)
              else return Nothing
  (dynMST, dynMS, dynMDR) <-
    if PF.stateFound testNames
    then do
      state <- FI.getFutState dl ctx $ PF.stateName testNames
      mt    <- FI.futGetStateField dl ctx state "maxtests"
      ms    <- FI.futGetStateField dl ctx state "maxsize"
      mdr   <- FI.futGetStateField dl ctx state "maxdiscardedratio"
      return (mt,ms,mdr)
    else return (100, 100, 100) -- move defaults to fut
  return $ MkState
    { stateTestName             = PF.ffTestName testNames
    , arbitrary                 = dynArb
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
        \n -> round $ toRational dynMS
              * (toRational n / toRational (dynMST - (if dynMST > 1 then 1 else 0)))
    , randomSeed                = gen
    }
