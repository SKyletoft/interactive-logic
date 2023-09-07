{-# LANGUAGE GHC2021, LambdaCase #-}

module Main where

import Prelude hiding (last)
import qualified Data.Maybe as Maybe

main :: IO ()
main = putStrLn "Hello"

todo :: a
todo = error "TODO"

data Law
  = AndEliminationLeft Int
  | AndEliminationRight Int
  | AndIntroduction Int Int

  | OrEliminationLeft Int Int
  | OrEliminationRight Int Int
  | OrIntroduction Int Int

  | ImplicationElimination Int Int
  | ImplicationIntroduction Int Int

  | NotIntroduction Int Int
  | NotElimination Int Int

  | NotBottom

  | DoubleNotIntroduction Int
  | DoubleNotElimination Int
  deriving (Show, Eq)

data Statement
  = And Statement Statement
  | Or Statement Statement
  | Not Statement
  | Implies Statement Statement
  | AssumptionBlock Statement [Statement]
  deriving (Show, Eq)

last :: [a] -> Maybe a
last [] = Nothing
last xs = Just . head . reverse $ xs

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
        let last' = Maybe.fromMaybe premise (last zz)
          in Just $ premise `Implies` last'
      _ -> Nothing
  DoubleNotIntroduction x -> Just . Not . Not $ ss !! x
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
  _ -> todo

check :: [Statement] -> Bool
check = todo
