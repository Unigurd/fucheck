module ParseFut ( FutFunNames
                , findTests
                , arbName
                , propName
                , condName
                , showName
                , stateName
                , labelName
                , ffTestName
                , stateFound
                , condFound
                , showFound
                , labelFound
                , only
                , without
                ) where

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

funNameMatches ("entry":actualName:_) get set ffns =
  if actualName == get ffns
  then Just $ set ffns
  else Nothing
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

arbName   ffnames = ffTestName ffnames ++ "arbitrary"
propName  ffnames = ffTestName ffnames ++ "property"
condName  ffnames = ffTestName ffnames ++ "condition"
showName  ffnames = ffTestName ffnames ++ "show"
stateName ffnames = ffTestName ffnames ++ "state"
labelName ffnames = ffTestName ffnames ++ "labels"
