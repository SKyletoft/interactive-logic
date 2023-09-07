{-# LANGUAGE GHC2021, LambdaCase #-}

module Main where

import qualified FFI.Termios as Termios

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified GHC.IO.Handle (hFlush)
import qualified GHC.IO.Handle.FD (stdout)

import qualified Prelude
import Prelude hiding (putStr, putStrLn, getChar)

f = do
  l <- parseLaw
  print l
  if Maybe.isJust l
    then f
    else return ()

main :: IO ()
main = do
  -- test
  f

i :: [IO Char]
i = getChar : i
  
input :: IO [Char]
input = sequence i

test :: IO ()
test = do
  i' <- input
  case i' of
    'a':'a':_ -> putStrLn "You wrote: aa"
    'a':'b':_ -> putStrLn "You wrote: ab"
    'a':'c':'c':[] -> putStrLn "You wrote: acc"
    'd':_ -> putStrLn "You wrote: d"
    _ -> putStrLn "Invalid input"
  return ()

-- Totally not cursed IO wrappers

getChar :: IO Char
getChar = do
  Termios.setupTerminal
  c <- Prelude.getChar
  Termios.deInitTerminal
  return c

putStr :: String -> IO ()
putStr s = do
  Prelude.putStr s
  GHC.IO.Handle.hFlush GHC.IO.Handle.FD.stdout

putStrLn :: String -> IO ()
putStrLn s = do
  Prelude.putStrLn s
  GHC.IO.Handle.hFlush GHC.IO.Handle.FD.stdout

todo :: a
todo = error "TODO"

parseLaw :: IO (Maybe Law)
parseLaw = do
  c <- getChar
  case c of
    'a' -> do
        putStr "Λ"
        c2 <- getChar
        case c2 of
          'l' -> do
            putStrLn "e1"
            fmap (Just . AndEliminationLeft) readLn
          'r' -> do
            putStrLn "e2"
            fmap (Just . AndEliminationRight) readLn
          'i' -> do
            putStrLn "i"
            fmap Just $ AndIntroduction <$> readLn <*> readLn
          _ -> do
            putStrLn " | Error"
            return Nothing
    'o' -> do
        putStr "V"
        c2 <- getChar
        case c2 of
          'l' -> do
            putStrLn "e1"
            fmap Just $ OrEliminationLeft <$> readLn <*> readLn
          'r' -> do
            putStrLn "e2"
            fmap Just $ OrEliminationRight <$> readLn <*> readLn
          'i' -> do
            putStrLn "i"
            fmap Just $ OrIntroduction <$> readLn <*> readLn
          _ -> do
            putStrLn " | Error"
            return Nothing
    _ -> do
      print . fromEnum $ c
      return Nothing

data Law
  = AndEliminationLeft Int
  | AndEliminationRight Int
  | AndIntroduction Int Int

  | OrEliminationLeft Int Int
  | OrEliminationRight Int Int
  | OrIntroduction Int Int

  | LEM Int

  | ImplicationElimination Int Int
  | ImplicationIntroduction Int Int

  | NotIntroduction Int Int
  | NotElimination Int Int

  | NotBottom
  | DeriveAnything Int Statement
  | Contradiction Int Int

  | DoubleNotIntroduction Int
  | DoubleNotElimination Int

  | ModusTollens Int Int
  deriving (Show, Eq)

data Statement
  = And Statement Statement
  | Or Statement Statement
  | Not Statement
  | Implies Statement Statement
  | AssumptionBlock Statement [Statement]
  | Variable Char
  | Bottom
  deriving (Show, Eq)

-- | Check if applying the law to the current set of statements is syntactically valid.
--   Does not check if it holds logically
checkStatement :: [Statement] -> Law -> Maybe Statement
checkStatement ss = \case
  AndEliminationLeft x ->
    case ss !! x of
      l `And` _ -> Just l
      _ -> Nothing
  AndEliminationRight x ->
    case ss !! x of
      _ `And` r -> Just r
      _ -> Nothing
  AndIntroduction x y ->
    Just $ (ss !! x) `And` (ss !! y)
  ImplicationIntroduction x y ->
    case ss !! x of
      AssumptionBlock premise zz ->
        let last'
              | List.null zz = premise
              | otherwise = last zz
          in Just $ premise `Implies` last'
      _ -> Nothing
  DoubleNotIntroduction x ->
    Just . Not . Not $ ss !! x
  DoubleNotElimination x ->
    case ss !! x of
      Not (Not x) -> Just x
      _ -> Nothing
  OrEliminationLeft x y ->
    case (ss !! x, ss !! y) of
      (l `Or` _, l') ->
        if l == l'
          then Just l
          else Nothing
      _ -> Nothing
  OrEliminationRight x y ->
    case (ss !! x, ss !! y) of
      (_ `Or` r, r') ->
        if r == r'
          then Just r
          else Nothing
      _ -> Nothing
  Contradiction x y ->
    case (ss !! x, ss !! y) of
      (x', Not y') ->
        if x' == y'
          then Just Bottom
          else Nothing
      (Not x', y') ->
        if x' == y'
          then Just Bottom
          else Nothing
      _ -> Nothing
  DeriveAnything l s ->
    case ss !! l of
      Bottom -> Just s
      _ -> Nothing
  LEM l ->
    let l' = ss !! l
     in Just $ l' `Or` Not l'
  -- _ -> todo

check :: [Statement] -> Bool
check = todo
