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
                , fixEntries
                , addStateGetters
                , stripComments
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

arbName   ffnames = ffTestName ffnames ++ "arbitrary"
propName  ffnames = ffTestName ffnames ++ "property"
condName  ffnames = ffTestName ffnames ++ "condition"
showName  ffnames = ffTestName ffnames ++ "show"
stateName ffnames = ffTestName ffnames ++ "state"
labelName ffnames = ffTestName ffnames ++ "labels"

stripComment [] = []
stripComment (word:rest) =
  if word `beginsWith` "--"
  then [] else stripComment rest
  where
    beginsWith (a:as) (b:bs) = a == b && beginsWith as bs
    beginsWith [] bs = False
    beginsWith _ _   = True

stripComments = unWordsLines . fmap stripComment . wordsLines
  where
    wordsLines   = fmap words . lines
    unWordsLines = unlines . fmap unwords

--fixEntries :: [FutFunNames] -> String -> String
--fixEntries tests programtext = unwords $ help tests $ words programtext
--  where
----    wordsLines   = fmap words . lines
----    unWordsLines = unlines . fmap unwords
--    help tests (binding:function:rest)
--      | binding == "let" || binding == "entry" =
--        if function `isIn` tests
--        then "entry":function:help tests rest
--        else "let"  :function:help tests rest
--      | otherwise = binding:function:help tests rest
--      where f `isIn` tests  = any ((==f) . ffTestName) tests
--    help tests (other:rest) = other:help tests rest
--    help _ [] = []


data Lada a = Cons a (Lada a) | Break (Lada a) | Nil
list2lada [] = Nil
list2lada (e:es) = Cons e $ list2lada es

str2lada :: String -> Lada String
str2lada = foldr (\elm acc -> comb (list2lada elm) (Break acc)) Nil . fmap words . lines
  where
    comb Nil la2 = la2
    comb (Cons e es) la2 = Cons e (comb es la2)
    comb (Break es)  la2 = Break (comb es la2)

lada2str :: Lada String -> String
lada2str Nil = []
lada2str (Break es) = '\n':lada2str es
lada2str (Cons e es) = e ++ " " ++ lada2str es

fixEntries :: [FutFunNames] -> String -> String
fixEntries tests programtext = lada2str $ fixer $ str2lada  programtext
  where
    fixer (Cons e es) =
      if e == "let" || e == "entry"
      then case (tests `contains`) <$> next es of
             Just True  -> Cons "entry" (fixer es)
             Just False -> Cons "let" (fixer es)
             Nothing    -> es
      else Cons e (fixer es)
    fixer (Break acc) = Break $ fixer acc
    fixer Nil = Nil

    tests `contains` name = any (name `beginsWith`) $ ffTestName <$> tests
    (a:as) `beginsWith` (b:bs) = if a == b
                                 then as `beginsWith` bs
                                 else False
    [] `beginsWith` (b:bs) = False
    _ `beginsWith` _ = True
    next Nil          = Nothing
    next (Break rest) = next rest
    next (Cons e _)   = Just e

--fixEntries :: [FutFunNames] -> String -> String
--fixEntries tests programtext = unWordsLines $ gybo $ wordsLines programtext
--  where
--    wordsLines   = fmap words . lines
--    unWordsLines = unlines . fmap unwords
--
--    gybo [] = []
--    gybo [[a]] = [[a]]
--    gybo ([]:rest) = []:gybo rest
--    gybo ([one]:(two:inners):outers) =
--      let (one',two') = f (one,two)
--      in [one]:gybo ((two':inners):outers)
--    gybo ((one:two:inners):outers) =
--      let (one', two') = f (one,two)
--          (head:rest) = gybo ((two':inners):outers)
--      in ((one':head):rest)
--
--    f (bind,name) =
--      if bind == "let" || bind == "empty"
--         && name `isIn` tests
--      then ("entry",name)
--      else ("let",  name)
--    name `isIn` tests = any (==name) $ ffTestName <$> tests


addStateGetters programtext =
  programtext
  ++ unlines [ "entry maxtests (state : state) : maxtests = state.maxtests"
             , "entry maxsize  (state : state) : maxsize = state.maxsize"
             , "entry maxdiscardedratio (state : state) : maxdiscardedratio = state.maxdiscardedratio"
             ]

