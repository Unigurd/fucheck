module ParseFut ( FutFunNames
                , findTests
                , arbName
                , propName
                , condName
                , showName
                , stateName
                , labelName
                , ffTestName
                , arbFound
                , propFound
                , stateFound
                , condFound
                , showFound
                , labelFound
                , only
                , without
                , usableTest
                , filtersplit
                , isFucheckFun
                ) where

import Debug.Trace (trace)
import Data.List (foldl')
import Control.Applicative ((<|>))

-- internal type for parsing tests in a futhark file
data FutFunNames = FutFunNames
  { ffTestName :: String
  , arbFound   :: Bool
  , propFound  :: Bool
  , condFound  :: Bool
  , showFound  :: Bool
  , stateFound :: Bool
  , labelFound :: Bool
  }

newFutFunNames name = FutFunNames
  { ffTestName = name
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

getTestName ["--", "fucheck", name] = Just name
getTestName _                       = Nothing

mapPerhaps :: (a -> Maybe a) -> [a] -> [a]
mapPerhaps f l = foldr (\elm acc -> case f elm of ; Nothing -> elm:acc ; Just newElm -> newElm:acc) [] l

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
  case getTestName line of
    Just newName -> newFutFunNames newName : foundFuns
    Nothing      -> mapPerhaps (anyFunNameMatches line) foundFuns

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
