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
