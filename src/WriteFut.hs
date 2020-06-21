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

module WriteFut (fixEntries, combineFutFuns, addStateGetters, entryTest) where
import Data.Char (isSpace)
import Data.List (dropWhileEnd, foldl')
import ParseFut ( FutFunNames(..)
                , ffTestName
                , isFucheckFun
                , arbName
                , propName
                , showName
                , condName
                , stateName
                , labelName
                )



data AttList a b = AttList [(a,b)] deriving Show

instance Functor (AttList a) where
  fmap f (AttList list) = AttList $ zip secret newShown
    where
      (secret,shown) = unzip list
      newShown = f <$> shown

-- Separate chunks of whitespace and chunks of non-whitespace
spaceWords :: String -> [String]
spaceWords (c1:c2:cs) =
  if isSpace c1 == isSpace c2 then
    (c1:r):rs
  else
    [c1] : spaceWords (c2:cs)
    where (r:rs) = spaceWords (c2:cs)
spaceWords [c] = [[c]]
spaceWords [] = [[]]

hideWhiteSpace string = AttList $ uncurry zip $ splitInTwo even
  where
    cleanButt = dropWhileEnd isSpace string
    separated = spaceWords cleanButt
    even      = if length separated `mod` 2 /= 0 then "":separated else separated
    splitInTwo []  = ([],[])
    splitInTwo (a:as) = (a:ys,xs)
      where (xs,ys) = splitInTwo as

mendWhiteSpace (AttList list) = foldr (\(white,word) acc -> white ++ word ++ acc) "" list

-- Turns entry into let
fixEntries :: [FutFunNames] -> String -> String
fixEntries tests programtext = mendWhiteSpace $ fix <$> hideWhiteSpace programtext
  where fix w = if w == "entry" then "let" else w

-- Combine futhark functions for type checking
-- doesn't work for some arrays
combineFutFuns :: FutFunNames -> String
combineFutFuns funs = result
  where
    name = ffTestName funs
    typecheck pred funname funtype  =
      if pred funs then
        "let typecheck_" ++ name ++ "_" ++ funname ++" (size :i32) (seed :i32) : " ++ funtype
        ++ " = " ++ funname ++ "_" ++ name ++ " (gen_" ++ name ++ " size seed)"
      else ""
    prop_comb   = typecheck propFound "prop" "bool"
    show_comb   = typecheck showFound "show" "[]u8"
    cond_comb   = typecheck condFound "cond" "bool"
    labels_comb = typecheck labelFound "labels" "[]u8"
    --state_comb  = typecheck stateFound "state" futStateDef
    result = unlines [prop_comb, show_comb, cond_comb, labels_comb] --, state_comb]

-- definition of a futhark state
futStateDef = "{ maxtests : maxtests , maxsize  : maxsize , maxdiscardedratio : maxdiscardedratio }"

-- Add state getters. duh.
addStateGetters =
  unlines [ "-- For accessing the state of each test"
          , "entry maxtests (state : " ++ futStateDef ++ ") : maxtests = state.maxtests"
          , "entry maxsize  (state : " ++ futStateDef ++ ") : maxsize = state.maxsize"
          , "entry maxdiscardedratio (state : " ++ futStateDef ++ ") : maxdiscardedratio = state.maxdiscardedratio"
          ]

sizeIndexes 1 = " sizes[0] "
sizeIndexes n = sizeIndexes (n-1) ++ "sizes[" ++ show (n-1) ++ "] "

-- Should generate unique names
-- Should not be dependent on Open Fucheck
entryFun test rettype f =
  "entry entry_" ++ f test ++ " (size : i32) (seed : i32) : " ++ rettype ++ " =\n"
  ++ "  let rng = rng_from_seed seed\n"
  ++ "  let (rng, sizes) = getsizes size rng " ++ show (numSizes test) ++ "\n"
  ++ "  in " ++ f test ++ " (" ++ arbName test ++ sizeIndexes (numSizes test) ++ "rng).1"


entryState test = "entry entry_" ++ stateName test ++ " : state = " ++ stateName test

entryTest test =
  unlines --[ if arbFound   test then entryGen    test else ""
          [ if propFound  test then entryFun test "bool" propName  else ""
          , if showFound  test then entryFun test "[]u8" showName  else ""
          , if condFound  test then entryFun test "bool" condName  else ""
          , if labelFound test then entryFun test "[]u8" labelName else ""
          , if stateFound test then entryState  test else ""
          ]
