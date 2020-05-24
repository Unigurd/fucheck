module Result ( Stage(..)
              , Result(..)
              , SingleResult(..)
              , stage2str
              ) where

import System.Random (randomIO, StdGen, getStdGen, next, RandomGen)
import qualified Data.Map.Strict as M

import FutInterface (CInt)
import State (State)

data Stage =
    Arb   {exitCode :: CInt}
  | Prop  {exitCode :: CInt}
  | Cond  {exitCode :: CInt}
  | Show  {exitCode :: CInt}
  | Label {exitCode :: CInt}

stage2str (Arb _)   = "arbitrary"
stage2str (Prop _)  = "property"
stage2str (Cond _)  = "condition"
stage2str (Show _)  = "show"
stage2str (Label _) = "label"

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
    , shownInput     :: Maybe (Either CInt String)
    , resultSeed     :: CInt
    }
  | GaveUp
    { resultTestName :: String
    , resultSeed     :: CInt
    , numTests       :: CInt
    }
  | Exception
    { resultTestName :: String
    , shownInput     :: Maybe (Either CInt String)
    , errorStage     :: Stage
    , resultSeed     :: CInt
    }

data SingleResult =
    SingleSuccess   State
  | SingleFailure   State (Maybe (Either CInt String))
  | SingleGaveUp    State
  | SingleException State Stage
