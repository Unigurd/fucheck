{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module FutServer where
import System.IO (hPutStrLn, hFlush, hGetLine, hClose, Handle)
import System.Process.Typed (Process, setStdin, setStdout, setStderr, createPipe, closed,
                             withProcessWait_, getStdin, getStdout, stopProcess)

import Data.Int (Int32)
import Text.Parsec (Parsec, letter, digit, many, char, oneOf, newline, getState,
                    putState, parse, string, manyTill, anyChar)
import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap, empty, (!?), insert)
import Control.Monad (liftM)
import Control.Monad.Trans.State (StateT(StateT))
import Control.Monad.IO.Class (liftIO)
import Data.MonoTraversable (MonoFoldable, Element)


import Conduit -- qualify!
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL

-- Bundling a process with the string read from it feels naughty
data Server     = Server (Process Handle Handle ()) String (HashMap String TypeProof)
data EntryPoint = EntryPoint String
data Type       = Type String
data Variable   = Variable String Type
data Command =
  Call
  | Restore
  | Store
  | Free
  | Inputs
  | Outputs
  | Clear
  | Pause_Profiling
  | Unpause_Profiling
  | Report


type MyString = String
type MyState  = HashMap MyString TypeProof

data O = O
data I = I

infixr :~
data TypeList t where
  (:~) :: a -> TypeList b -> TypeList (a,b)
  TNil  :: TypeList ()

data TypeProof = forall t. TypeProof (TypeList t)

proveChar :: Char ->  TypeProof
proveChar c = aux (TypeProof TNil) $ fromEnum c
  where
    aux :: TypeProof -> Int -> TypeProof
    aux (TypeProof c) 0 = TypeProof (O :~ c)
    aux (TypeProof c) 1 = TypeProof (I :~ c)
    aux (TypeProof c) n
      | n `mod` 2 == 0 = aux (TypeProof (O :~ c)) (n `div` 2)
      | otherwise      = aux (TypeProof (I :~ c)) ((n-1) `div` 2)

proveOpaqueType :: String -> TypeProof
proveOpaqueType s = aux (TypeProof TNil) s
  where
    aux :: TypeProof -> [Char] -> TypeProof
    aux s@(TypeProof _) []     = s
    aux   (TypeProof s) (c:cs) =
      case proveChar c of
        (TypeProof tc) -> aux (TypeProof (tc :~ s)) cs

parseOpaqueType :: Parsec MyString MyState TypeProof
parseOpaqueType = do
  l  <- letter
  ls <- many $ letter <|> digit <|> oneOf "_'(,)[]"
  let str = (l:ls)
  s  <- getState
  case s !? str of
    Just p  -> return p
    Nothing -> do
      let p = proveOpaqueType (l:ls)
      putState $ insert str p s
      return p

parseTypes :: Parsec MyString MyState TypeProof
parseTypes = do
  t  <- parseOpaqueType
  newline
  TypeProof ts <- parseTypes
  return $ TypeProof $ t :~ ts

countTypes :: TypeProof -> Int
countTypes t = aux 0 t
  where
    aux :: Int -> TypeProof -> Int
    aux n (TypeProof (_ :~ ts)) = aux (n+1) (TypeProof ts)
    aux n (TypeProof TNil)      = n

type ServerInteraction = StateT Server IO

-- Parses to Nothing if the process signals OK and (Just errmsg) otherwise
okOrFailure :: Parsec MyString a (Maybe String)
okOrFailure = do
  string "%%% "
  s <- okP <|> failureP
  if s
    then return Nothing
    else do e <- manyTill anyChar $ string ok
            return $ Just e
  where
    ok       = "%%% OK\n"
    okP      = string "OK\n"      >> return True
    failureP = string "FAILURE\n" >> return False


-- futserver = setStdOut createSource $ setStdIn createSinkClose $ proc test-prog [args, here]
-- withProcessWait futserver \p -> ... -- eller withProcessTerm? eller bracketP?
-- yieldmany . f . sourceToList
-- map


readServer :: ( Monad m
              , MonoFoldable mono
              , Element mono ~ Char
              )
           => ConduitT Char Char m (Either [String] [String])
readServer = auxRead []
  where
    auxRead :: ( Monad m0
               , MonoFoldable mono0
               , Element mono0 ~ Char
               )
            => [String] -> ConduitT mono0 mono0 m0 (Either [String] [String])
    auxRead ls = do
      l <- lineC $ foldMapC (:[])
      case l of
        "%%% OK"      -> return $ Right $ reverse ls
        "%%% FAILURE" -> (return . (either Left Left)) (Right ["Davs"]) -- =<< auxRead ls
        _             -> return (Left ["hej"]) -- --auxRead (l:ls)

-- conLine = do
--   lineC
