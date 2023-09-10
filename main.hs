{-# LANGUAGE GHC2021    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified FFI.Termios      as Termios

import qualified Control.Monad    as Monad

import qualified Data.List        as List
import qualified Data.Maybe       as Maybe

import qualified GHC.IO.Handle    (hFlush)
import qualified GHC.IO.Handle.FD (stdout)

import           Prelude          hiding (getChar, putStr, putStrLn)
import qualified Prelude

import qualified Grammar.Par as Par
import qualified Grammar.Abs as Abs
import Grammar.Abs (Exp(..))

----------------------------- OPERATORS -----------------------------
-- Copied from Data.List.Extra
(!?) :: [a] -> Int -> Maybe a
[] !? _     = Nothing
(x:_) !? 0  = Just x
(_:xs) !? n = xs !? (n - 1)

(-->) :: Bool -> Bool -> Bool
True --> False = False
_ --> _        = True

---------------------------- IO WRAPPERS ----------------------------

parseLn :: IO Statement
parseLn = fmap parseStatement getLine

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

------------------------------ HELPERS ------------------------------
todo :: a
todo = error "TODO"

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

---------------------------- ACTUAL CODE ----------------------------
main :: IO ()
main = do
  putStrLn "Proof editor"
  repl True 0 []
  return ()

repl :: Bool -> Int -> [(Statement, Law)] -> IO [(Statement, Law)]
repl skipBackLog startingAt ss = do
  Monad.unless skipBackLog $ do putStrLn . display $ ss
    -- putStrLn $
    --   if check . fst . last $ ss
    --     then "Valid"
    --     else "Contradiction"
  law <- parseLaw
  case law of
    Nothing -> repl True startingAt ss
    Just Quit -> do
      putStrLn "------------------------------------------"
      putStrLn . display $ ss
      print ss
      error "Exit"
    Just (AssumptionIntroduction s) -> do
      assumption <-
        repl False (length ss) (ss ++ [(s, AssumptionIntroduction s)])
      let assumptionBlock = AssumptionBlock s assumption
      let implication = Implies s (fst . last $ assumption)
      repl
        False
        startingAt
        (ss ++
         replicate (length assumption - 1) (FillerS, FillerL) ++
         [ (assumptionBlock, AssumptionIntroduction s)
         , (implication, ImplicationIntroduction (length ss))
         ])
    Just (ImplicationIntroduction _) ->
      if startingAt == 0
        then do
          putStr "Invalid application of rule\r"
          repl True startingAt ss
        else return . drop startingAt $ ss
    Just law' -> do
      case checkStatement (map fst ss) law' of
        Nothing -> do
          putStr "Invalid application of rule\r"
          repl True startingAt ss
        Just s' -> repl False startingAt (ss ++ [(s', law')])

parseLaw :: IO (Maybe Law)
parseLaw = do
  c <- getChar
  putStr $ replicate 50 ' ' ++ "\r"
  case c of
    'a' -> do
      putStr "Λ"
      c2 <- getChar
      case c2 of
        'l' -> do
          putStr "e1: "
          fmap (Just . AndEliminationLeft) readLn
        'r' -> do
          putStr "e2: "
          fmap (Just . AndEliminationRight) readLn
        'i' -> do
          putStrLn "i"
          fmap Just $ AndIntroduction <$> readLn <*> readLn
        _ -> do
          putStrLn " | Error"
          return Nothing
    'c' -> do
      putStrLn "Contradiction:"
      fmap Just $ Contradiction <$> readLn <*> readLn
    'i' -> do
      putStr "→"
      c2 <- getChar
      case c2 of
        'i' -> do
          putStrLn "i"
          return $ Just (ImplicationIntroduction 0)
        'e' -> do
          putStrLn "e"
          fmap Just $ ImplicationElimination <$> readLn <*> readLn
        _ -> do
          putStrLn " | Error"
          return Nothing
    'l' -> do
      putStr "LEM: "
      fmap (Just . LEM) readLn
    'm' -> do
      putStrLn "MT"
      fmap Just $ ModusTollens <$> readLn <*> readLn
    'n' -> do
      putStr "¬"
      c2 <- getChar
      case c2 of
        'i' -> do
          putStrLn "i"
          fmap Just $ NotIntroduction <$> readLn
        'e' -> do
          putStrLn "e"
          fmap Just $ NotElimination <$> readLn <*> readLn
        'b' -> do
          putStrLn "b"
          return $ Just NotBottom
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
    'p' -> do
      putStr "Premise: "
      fmap (Just . Premise) readLn
    'q' -> return $ Just Quit
    's' -> do
      putStr "Assumption: "
      fmap (Just . AssumptionIntroduction) readLn
    '\127' -> do
      putStr "\r                             \r"
      return Nothing
    '\4' -> do
      error "Exit"
    _ -> do
      putStr . show . fromEnum $ c
      return Nothing

data Law
  = AndEliminationLeft Int
  | AndEliminationRight Int
  | AndIntroduction Int Int
  | AssumptionIntroduction Statement
  | Contradiction Int Int
  | DeriveAnything Int Statement
  | DoubleNotIntroduction Int
  | DoubleNotElimination Int
  | ImplicationElimination Int Int
  | ImplicationIntroduction Int
  | LEM Int
  | ModusTollens Int Int
  | NotIntroduction Int
  | NotElimination Int Int
  | NotBottom
  | OrEliminationLeft Int Int
  | OrEliminationRight Int Int
  | OrIntroduction Int Int
  | Premise Statement
  | Quit
  | FillerL
  deriving (Show, Read, Eq)

data Statement
  = And Statement Statement
  | Or Statement Statement
  | Not Statement
  | Implies Statement Statement
  | AssumptionBlock Statement [(Statement, Law)]
  | Variable Char
  | Bottom
  | FillerS
  deriving (Show, Read, Eq)

convert :: Exp -> Statement
convert = \case
  EPar e -> convert e
  EBtm -> Bottom
  EAnd l r -> And (convert l) (convert r)
  EOr l r -> Or (convert l) (convert r)
  EImpl l r -> Implies (convert l) (convert r)
  ENot e -> Not (convert e)
  EVar (Abs.Ident [n]) -> Variable n
  EVar _ -> error "Stick to one letter variables"

parseStatement :: String -> Statement
parseStatement s = convert bnfcOut
  where
    Right bnfcOut = Par.pExp $ Par.myLexer s

prettyShow :: Statement -> String
prettyShow =
  \case
    l `And` r -> wrap $ prettyShow l ++ " Λ " ++ prettyShow r
    l `Or` r -> wrap $ prettyShow l ++ " V " ++ prettyShow r
    Not l -> "¬" ++ prettyShow l
    l `Implies` r -> wrap $ prettyShow l ++ " → " ++ prettyShow r
    AssumptionBlock _ ss -> "[]"
    Variable c -> [c]
    Bottom -> "⊥"
    FillerS -> "(Ignore me)"
  where
    wrap s = "(" ++ s ++ ")"

prettyShowLaw :: Law -> String
prettyShowLaw =
  \case
    AndEliminationLeft x -> "Λel " ++ show x
    AndEliminationRight x -> "Λer " ++ show x
    AndIntroduction x y -> "Λi " ++ show x ++ ", " ++ show y
    Contradiction x y -> "⊥ " ++ show x ++ ", " ++ show y
  -- DeriveAnything Int Statement
  -- DoubleNotIntroduction Int
  -- DoubleNotElimination Int
    ImplicationElimination x y -> "→e " ++ show x ++ ", " ++ show y
    ImplicationIntroduction x -> "→i" ++ show x
    LEM x -> "LEM " ++ show x
  -- ModusTollens Int Int
  -- NotIntroduction Int Int
  -- NotElimination Int Int
  -- NotBottom
  -- OrEliminationLeft Int Int
  -- OrEliminationRight Int Int
  -- OrIntroduction Int Int
    Premise _ -> "Premise"
    AssumptionIntroduction _ -> "Assumption"
    NotIntroduction x -> "Syntax change " ++ show x
    FillerL -> "(Ignore me)"
    s -> error . show $ s

display :: [(Statement, Law)] -> String
display =
  unlines .
  map (\(n, (a, b)) -> pad 5 (show n) ++ " | " ++ pad 30 a ++ b) .
  zip [0 ..] . displayInner
  where
    pad l s = s ++ replicate (l - length s) ' '
    doLine (s, l) =
      case s of
        FillerS              -> []
        AssumptionBlock _ ss -> map (first ("| " ++)) . displayInner $ ss
        _                    -> [(prettyShow s, prettyShowLaw l)]
    displayInner = concatMap doLine

-- | Check if applying the law to the current set of statements is
--   syntactically valid.
--   Does not check if it holds ally
checkStatement :: [Statement] -> Law -> Maybe Statement
checkStatement ss =
  \case
    AndEliminationLeft x -> do
      x' <- ss !? x
      case x' of
        l `And` _ -> Just l
        _         -> Nothing
    AndEliminationRight x -> do
      x' <- ss !? x
      case x' of
        _ `And` r -> Just r
        _         -> Nothing
    AndIntroduction x y -> do
      x' <- ss !? x
      y' <- ss !? y
      return $ x' `And` y'
    -- ImplicationIntroduction x y -> do
    --   x' <- ss !? x
    --   case x' of
    --     AssumptionBlock premise zz ->
    --       let last'
    --             | List.null zz = premise
    --             | otherwise = last zz
    --        in Just $ premise `Implies` last'
    --     _ -> Nothing
    DoubleNotIntroduction x -> do
      x' <- ss !? x
      return . Not . Not $ x'
    DoubleNotElimination x ->
      case ss !? x of
        Just (Not (Not x)) -> Just x
        _                  -> Nothing
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
        _      -> Nothing
    LEM l -> do
      l' <- ss !? l
      Just $ l' `Or` Not l'
    Premise s -> Just s
    ImplicationIntroduction _ -> do
      error "hi"
    ImplicationElimination l r -> do
      l' <- ss !? l
      r' <- ss !? r
      case (l', r') of
        (x, x' `Implies` y) ->
          if x == x'
            then Just y
            else Nothing
        (x' `Implies` y, x) ->
          if x == x'
            then Just y
            else Nothing
        _ -> Nothing
    NotBottom -> return . Not . Not $ Bottom
    NotIntroduction l -> do
      case ss !? l of
        Just (x `Implies` Bottom) -> Just $ Not x
        _                         -> Nothing
    s -> error . show $ s

check :: Statement -> Bool
check =
  \case
    l `And` r -> check l && check r
    l `Or` r -> check l || check r
    Not l -> not $ check l
    l `Implies` r -> check l --> check r
    AssumptionBlock _ _ -> todo
    Variable _ -> True
    Bottom -> False
