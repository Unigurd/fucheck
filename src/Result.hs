module Result ( Stage(..)
              , Result(..)
--              , SingleResult(..)
              , stage2str
              ) where

import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import qualified Data.Map.Strict as M

import FutInterface (CInt, Stage(..), stage2str)
import State (State, stateTestName, getSeed)



data Result =
    Success
    { resultTestName :: String
    , numTests       :: CInt
    , resultLabels   :: Maybe (M.Map String CInt)
    }
  | Failure
    { resultTestName :: String
    -- Nothing if no attempt at showing could be made
    -- Just Left if it tried to generate a string but failed
    -- Just Right if a string was successfully generated
    , shownInput     :: Maybe String
    , resultSeed     :: CInt
    }
  | GaveUp
    { resultTestName :: String
    , resultSeed     :: CInt
    , numTests       :: CInt
    }
  | Exception
    { resultTestName :: String
    , shownInput     :: Maybe String
    , errorStage     :: Stage
    , resultSeed     :: CInt
    }
