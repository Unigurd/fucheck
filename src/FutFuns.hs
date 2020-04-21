module FutFuns ( FutFuns(..)
               , ffTestName
               , loadFutFuns
               , findTests
               ) where

import Foreign.C.Types (CInt(CInt))
import Control.Monad.Trans.Except(ExceptT(ExceptT),runExceptT)
import Foreign.Ptr (Ptr)
import Data.List (foldl')

import FutInterface ( FutharkTestData
                    , FutState
                    , mkArbitrary
                    , mkProperty
                    , mkShow
                    , getFutState
                    , futMaxTests
                    , CInt
                    )

data FutFuns = MkFuns
  { futArb   :: CInt -> CInt -> ExceptT CInt IO (Ptr FutharkTestData)
  , futProp  :: Ptr FutharkTestData -> ExceptT CInt IO Bool
  , futShow  :: Maybe (Ptr FutharkTestData -> ExceptT CInt IO String)
  , futMaxSuccessTests :: CInt
  }

-- internal type for parsing tests in a futhark file
data FutFunNames = FutFunNames
  { ffTestName :: String
  , arbFound   :: Bool
  , propFound  :: Bool
  , showFound  :: Bool
  , stateFound :: Bool
  }

arbName   ffnames = ffTestName ffnames ++ "arbitrary"
propName  ffnames = ffTestName ffnames ++ "property"
showName  ffnames = ffTestName ffnames ++ "show"
stateName ffnames = ffTestName ffnames ++ "state"

newFutFunNames name = FutFunNames
  { ffTestName = name
  , arbFound   = False
  , propFound  = False
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
  dynShow <- if showFound testNames
             then Just <$> mkShow dl ctx (showName testNames)
             else return Nothing
  dynMaxSuccessTests <- if stateFound testNames
                        then futMaxTests dl ctx =<< (getFutState dl ctx $ stateName testNames)
                        else return 100
  return MkFuns { futArb   = dynArb
                , futProp  = dynProp
                , futShow  = dynShow
                , futMaxSuccessTests = dynMaxSuccessTests
                }

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = foldr (\elm acc -> case f elm of
                          Just x  -> x:acc
                          Nothing -> acc) []

getTestName ["--", "fucheck", name] = Just name
getTestName _                       = Nothing

mapPerhaps :: (a -> Maybe a) -> [a] -> [a]
mapPerhaps f l = foldr (\elm acc -> case f elm of ; Nothing -> elm:acc ; Just newElm -> newElm:acc) [] l

funNameMatches ("entry":actualName:_) expectedName = actualName == expectedName
funNameMatches _ _ = False

anyFunNameMatches line ffns =
  if matchesLine $ arbName ffns
  then Just $ ffns {arbFound = True}
  else if matchesLine $ propName ffns
       then Just $ ffns {propFound = True}
       else if matchesLine $ showName ffns
            then Just $ ffns {showFound = True}
            else if matchesLine $ stateName ffns
                 then Just $ ffns {stateFound = True}
                 else Nothing
  where matchesLine = funNameMatches line


checkLine foundFuns line =
  case getTestName line of
    Just newName -> newFutFunNames newName : foundFuns
    Nothing      -> mapPerhaps (anyFunNameMatches line) foundFuns

