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

{-# LANGUAGE ScopedTypeVariables #-}
module ParseFut ( FutFunNames(..)
                , findTests
                , arbName
                , propName
                , condName
                , showName
                , stateName
                , labelName
                , only
                , without
                , usableTest
                , filtersplit
                , isFucheckFun
                ) where

import Debug.Trace (trace)
import Data.List (foldl')
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

-- internal type for parsing tests in a futhark file
data FutFunNames = FutFunNames
  { ffTestName :: String
  , numSizes   :: Integer
  , arbFound   :: Bool
  , propFound  :: Bool
  , condFound  :: Bool
  , showFound  :: Bool
  , stateFound :: Bool
  , labelFound :: Bool
  }

newFutFunNames name n = FutFunNames
  { ffTestName = name
  , numSizes   = n
  , arbFound   = False
  , propFound  = False
  , condFound  = False
  , showFound  = False
  , stateFound = False
  , labelFound = False
  }


filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr (\elm acc -> case f elm of
                          Just x  -> x:acc
                          Nothing -> acc) []

-- get name of test
getTestName ("--":"fucheck":name:_) = Just name
getTestName _                       = Nothing

getNumSizes ("--":"fucheck":_:num:_) =
  case (readMaybe num, (0<) <$> readMaybe num) of
    (Just n, Just True) -> Just $ n + 1
    _ -> Nothing -- trace ((show $ (readMaybe num :: Maybe Integer)) ++ " " ++ num) Nothing
getNumSizes _ = Nothing

mapPerhaps :: (a -> Maybe a) -> [a] -> [a]
mapPerhaps f l =
  foldr (\elm acc ->
           case f elm of
             Nothing -> elm:acc
             Just newElm -> newElm:acc
        ) [] l

funNameMatches (binding:actualName:_) get set ffns
  | binding == "let" || binding == "entry" =
    if actualName == get ffns
    then Just $ set ffns
    else Nothing
  | otherwise = Nothing
funNameMatches _ _ _ _ = Nothing


anyFunNameMatches :: [String] -> FutFunNames -> Maybe FutFunNames
anyFunNameMatches line ffns =
      matchesLine arbName   (\f -> f {arbFound   = True}) ffns
  <|> matchesLine propName  (\f -> f {propFound  = True}) ffns
  <|> matchesLine condName  (\f -> f {condFound  = True}) ffns
  <|> matchesLine showName  (\f -> f {showFound  = True}) ffns
  <|> matchesLine stateName (\f -> f {stateFound = True}) ffns
  <|> matchesLine labelName (\f -> f {labelFound = True}) ffns
  where matchesLine = funNameMatches line


checkLine foundFuns line =
  case (getTestName line, getNumSizes line) of
    (Just newName, Just n)  -> newFutFunNames newName n : foundFuns
    (Just newName, Nothing) -> newFutFunNames newName 1 : foundFuns
    (Nothing, _)            -> mapPerhaps (anyFunNameMatches line) foundFuns

only    names foundFuns = [fun | fun <- foundFuns, name <- names, ffTestName fun == name]
without names foundFuns = [fun | fun <- foundFuns, all (/= ffTestName fun) names]

findTests :: String -> [FutFunNames]
findTests source = tests
  where
    tokens = words <$> lines source
    -- breaks if using foldr to preserve test order
    tests  = reverse $ foldl' checkLine [] tokens
      --filterMap getTestName tokens

usableTest test = arbFound test && propFound test
filtersplit p list = foldr (\elm (ts,fs) -> if p elm then (elm:ts,fs) else (ts,elm:fs)) ([],[]) list


arbName   ffnames = "gen_"    ++ ffTestName ffnames
propName  ffnames = "prop_"   ++ ffTestName ffnames
condName  ffnames = "cond_"   ++ ffTestName ffnames
showName  ffnames = "show_"   ++ ffTestName ffnames
stateName ffnames = "state_"  ++ ffTestName ffnames
labelName ffnames = "labels_" ++ ffTestName ffnames

isFucheckFun name test =
  name == arbName test
  || name == propName test
  || name == condName test
  || name == showName test
  || name == stateName test
  || name == labelName test
