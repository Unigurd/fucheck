module Result ( Stage(..)
              , Result(..)
              , stage2str
              ) where

import System.Exit (ExitCode(ExitSuccess), exitSuccess, exitFailure)
import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)

import FutInterface (CInt)
import State (State)

data Stage = Arb | Prop | Show
stage2str Arb  = "arbitrary"
stage2str Prop = "property"
stage2str Show = "show"

data Result =
    Success
    { resultTestName :: String
    , numTests       :: Integer
    }
  | Failure
    -- Nothing if no attempt at showing could be made
    -- Just Left if it tried to generate a string but failed
    -- Just Right if a string was successfully generated
    { resultTestName :: String
    , shownInput     :: Maybe (Either CInt String)
    , resultSeed     :: CInt
    }
  | Exception
    { resultTestName :: String
    , shownInput     :: Maybe (Either CInt String)
    , errorStage     :: Stage
    , futExitCode    :: CInt
    , resultSeed     :: CInt
    }










