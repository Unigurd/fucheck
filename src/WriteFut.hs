module WriteFut (fixEntries, combineFutFuns, addStateGetters) where
import ParseFut ( FutFunNames(..), ffTestName, isFucheckFun )



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

    tests `contains` testname = any (isFucheckFun testname) tests


    next Nil          = Nothing
    next (Break rest) = next rest
    next (Cons e _)   = Just e

futStateDef = "{ maxtests : maxtests , maxsize  : maxsize , maxdiscardedratio : maxdiscardedratio }"

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

addStateGetters =
  unlines [ "-- For accessing the state of each test"
          , "entry maxtests (state : " ++ futStateDef ++ ") : maxtests = state.maxtests"
          , "entry maxsize  (state : " ++ futStateDef ++ ") : maxsize = state.maxsize"
          , "entry maxdiscardedratio (state : " ++ futStateDef ++ ") : maxdiscardedratio = state.maxdiscardedratio"
          ]
