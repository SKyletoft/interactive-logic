{-# LANGUAGE GHC2021, LambdaCase #-}

module Main where

import qualified FFI.Termios as Termios

import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified GHC.IO.Handle (hFlush)
import qualified GHC.IO.Handle.FD (stdout)

import qualified Prelude
import Prelude hiding (putStr, putStrLn, getChar)


--------------------- OPERATORS ----------------------

-- Copied from Data.List.Extra
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

(-->) :: Bool -> Bool -> Bool
True --> False = False
_ --> _ = True


-------------------- IO WRAPPERS ---------------------

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


--------------------- HELPERS ------------------------

todo :: a
todo = error "TODO"


--------------------- ACTUAL CODE --------------------

main :: IO ()
main = repl []

repl :: [Statement] -> IO ()
repl ss = do
  putStrLn . unlines . map prettyShow $ ss
  putStrLn $ if check $ last ss
    then "Valid"
    else "Contradiction"
  law <- parseLaw
  if Maybe.isNothing law
    then do
      putStrLn "Bad input"
      repl ss
    else do
      let s = checkStatement ss (Maybe.fromJust law)
      if Maybe.isNothing s
        then do
          putStrLn "Bad law"
          repl ss
        else repl (ss ++ [Maybe.fromJust s])

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

parse :: String -> Statement
parse = todo

prettyShow :: Statement -> String
prettyShow = \case
  l `And` r -> "(" ++ prettyShow l ++ " Λ " ++ prettyShow r ++ ")"
  l `Or` r -> "(" ++ prettyShow l ++ " V " ++ prettyShow r ++ ")"
  Not l -> "¬" ++ prettyShow l
  l `Implies` r -> "(" ++ prettyShow l ++ " → " ++ prettyShow r ++ ")"
  AssumptionBlock s ss -> unlines $ map (("| " ++ ) . prettyShow) (s:ss)
  Variable c -> [c]
  Bottom -> "⊥"

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
  AndIntroduction x y -> do
    x' <- ss !? x
    y' <- ss !? y
    return $ x' `And` y'
  ImplicationIntroduction x y ->
    case ss !! x of
      AssumptionBlock premise zz ->
        let last'
              | List.null zz = premise
              | otherwise = last zz
          in Just $ premise `Implies` last'
      _ -> Nothing
  DoubleNotIntroduction x -> do
    x' <- ss !? x
    return . Not . Not $ x'
  DoubleNotElimination x ->
    case ss !! x of
      Not (Not x) -> Just x
      _ -> Nothing
  OrEliminationLeft x y -> do
    x' <- ss !? x
    y' <- ss !? y
    case (x', y') of
      (l `Or` _, l') ->
        if l == l'
          then Just l
          else Nothing
      _ -> Nothing
  OrEliminationRight x y -> do
    x' <- ss !? x
    y' <- ss !? y
    case (x', y') of
      (_ `Or` r, r') ->
        if r == r'
          then Just r
          else Nothing
      _ -> Nothing
  Contradiction x y -> do
    x' <- ss !? x
    y' <- ss !? y
    case (x', y') of
      (x', Not y') ->
        if x' == y'
          then Just Bottom
          else Nothing
      (Not x', y') ->
        if x' == y'
          then Just Bottom
          else Nothing
      _ -> Nothing
  DeriveAnything l s -> do
    l' <- ss !? l
    case l' of
      Bottom -> Just s
      _ -> Nothing
  LEM l ->
    let l' = ss !! l
     in Just $ l' `Or` Not l'

check :: Statement -> Bool
check = \case
  l `And` r -> check l && check r
  l `Or` r -> check l || check r
  Not l -> not $ check l
  l `Implies` r -> check l --> check r
  AssumptionBlock _ _ -> True
  Variable _ -> True
  Bottom -> False
