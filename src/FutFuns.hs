module FutFuns ( FutFuns(..)
               , ffTestName
               , loadFutFuns
               , findTests
               ) where

import Foreign.C.Types (CInt(CInt))
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import Foreign.Ptr (Ptr)
import Data.List (foldl')
import Control.Applicative ((<|>))

import FutInterface ( FutharkTestData
                    , FutState
                    , mkArbitrary
                    , mkProperty
                    , mkShow
                    , getFutState
                    , futGetStateField
                    , CInt
                    )

data FutFuns = MkFuns
  { futArb               :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , futProp              :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , futCond              :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO Bool)
  , futShow              :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  , futMaxSuccessTests   :: CInt
  , futMaxSize           :: CInt
  , futMaxDiscardedRatio :: CInt
  }

-- internal type for parsing tests in a futhark file
data FutFunNames = FutFunNames
  { ffTestName :: String
  , arbFound   :: Bool
  , propFound  :: Bool
  , condFound  :: Bool
  , showFound  :: Bool
  , stateFound :: Bool
  }

arbName   ffnames = ffTestName ffnames ++ "arbitrary"
propName  ffnames = ffTestName ffnames ++ "property"
condName  ffnames = ffTestName ffnames ++ "condition"
showName  ffnames = ffTestName ffnames ++ "show"
stateName ffnames = ffTestName ffnames ++ "state"

newFutFunNames name = FutFunNames
  { ffTestName = name
  , arbFound   = False
  , propFound  = False
  , condFound  = False
  , showFound  = False
  , stateFound = False
  }

findTests :: String -> [FutFunNames]
findTests source = tests
  where
    tokens = words <$> lines source
    -- breaks if using foldr to preserve test order
    tests  = reverse $ foldl' checkLine [] tokens
      --filterMap getTestName tokens

loadFutFuns dl ctx testNames = do
  dynArb  <- mkArbitrary dl ctx $ arbName testNames
  dynProp <- mkProperty  dl ctx $ propName testNames
  dynCond <- if condFound testNames
             then Just <$> mkProperty dl ctx (condName testNames)
             else return Nothing
  dynShow <- if showFound testNames
             then Just <$> mkShow dl ctx (showName testNames)
             else return Nothing
  (dynMST, dynMS, dynMDR) <-
    if stateFound testNames
    then do
      state <- (getFutState dl ctx $ stateName testNames)
      mt <- futGetStateField dl ctx state "maxtests"
      ms <- futGetStateField dl ctx state "maxsize"
      mdr <- futGetStateField dl ctx state "maxdiscardedratio"
      return (mt,ms,mdr)
    else return (100, 100, 100) -- move defaults to fut
  return MkFuns { futArb               = dynArb
                , futProp              = dynProp
                , futShow              = dynShow
                , futCond              = dynCond
                , futMaxSuccessTests   = dynMST
                , futMaxSize           = dynMS
                , futMaxDiscardedRatio = dynMDR
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

anyFunNameMatches line ffns =
  matchesLine arbName (\f -> f {arbFound = True}) ffns
  <|> matchesLine propName (\f -> f {propFound = True}) ffns
  <|> matchesLine condName (\f -> f {condFound = True}) ffns
  <|> matchesLine showName (\f -> f {showFound = True}) ffns
  <|> matchesLine stateName (\f -> f {stateFound = True}) ffns
  where matchesLine = funNameMatches line


checkLine foundFuns line =
  case getTestName line of
    Just newName -> newFutFunNames newName : foundFuns
    Nothing      -> mapPerhaps (anyFunNameMatches line) foundFuns

