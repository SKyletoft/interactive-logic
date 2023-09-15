{-# LANGUAGE GHC2021    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified FFI.Termios      as Termios

import qualified Control.Monad    as Monad

import qualified Data.List        as List
import qualified Data.Maybe       as Maybe
import qualified Data.Set         as Set

import qualified GHC.IO.Handle    (hFlush)
import qualified GHC.IO.Handle.FD (stdout)

import           Prelude          hiding (getChar, putStr, putStrLn, readLn)
import qualified Prelude

import           Data.Maybe       (isJust)
import           Grammar.Abs      (Exp (..))
import qualified Grammar.Abs      as Abs
import qualified Grammar.Par      as Par

import qualified Text.Read

type S a = Set.Set a

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

tryReadLn :: Read a => IO (Maybe a)
tryReadLn = fmap Text.Read.readMaybe getLine

tryParseLn :: IO (Maybe Statement)
tryParseLn = fmap parseStatement getLine

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
      putStrLn $ replicate 50 '-'
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
    Just (Search s) -> do
      let searchResult = bfs ((== s) . fst . last) applicableRules [ss]
      putStrLn . display $ searchResult
      repl False startingAt ss
    Just law' -> do
      case checkStatement (map fst ss) law' of
        Nothing -> do
          putStr "Invalid application of rule\r"
          repl True startingAt ss
        Just s' -> repl False startingAt (ss ++ [(s', law')])

parseLaw :: IO (Maybe Law)
parseLaw = do
  let printOnError :: Maybe a -> IO (Maybe a)
      printOnError =
        \case
          Nothing -> putStrLn "Parse error" >> return Nothing
          x -> return x
  let joinIO :: IO (Maybe a) -> IO (Maybe b) -> IO (Maybe (a, b))
      joinIO a b = do
        a' <- a
        case a' of
          Nothing -> return Nothing
          Just a'' -> do
            b' <- b
            case b' of
              Nothing  -> return Nothing
              Just b'' -> return . Just $ (a'', b'')
  let readOne :: (a -> Law) -> IO (Maybe a) -> IO (Maybe Law)
      readOne l io = do
        i <- printOnError =<< io
        return $ fmap l i
  let readTwo ::
           (a -> b -> Law) -> IO (Maybe a) -> IO (Maybe b) -> IO (Maybe Law)
      readTwo l io1 io2 = do
        i <- joinIO (printOnError =<< io1) (printOnError =<< io2)
        return $ uncurry l <$> i
  c <- getChar
  putStr $ replicate 50 ' ' ++ "\r"
  case c of
    'a' -> do
      putStr "Λ"
      c2 <- getChar
      case c2 of
        'l' -> do
          putStr "e1: "
          readOne AndEliminationLeft tryReadLn
        'r' -> do
          putStr "e2: "
          readOne AndEliminationRight tryReadLn
        'i' -> do
          putStrLn "i"
          readTwo AndIntroduction tryReadLn tryReadLn
        _ -> do
          putStrLn " | Error"
          return Nothing
    'c' -> do
      putStrLn "Contradiction:"
      readTwo Contradiction tryReadLn tryReadLn
    'i' -> do
      putStr "→"
      c2 <- getChar
      case c2 of
        'i' -> do
          putStrLn "i"
          return $ Just (ImplicationIntroduction 0)
        'e' -> do
          putStrLn "e"
          readTwo ImplicationElimination tryReadLn tryReadLn
        _ -> do
          putStrLn " | Error"
          return Nothing
    'l' -> do
      putStr "LEM: "
      readOne LEM tryReadLn
    'm' -> do
      putStrLn "MT"
      readTwo ModusTollens tryReadLn tryReadLn
    'n' -> do
      putStr "¬"
      c2 <- getChar
      case c2 of
        'i' -> do
          putStrLn "i"
          readOne NotIntroduction tryReadLn
        'e' -> do
          putStrLn "e"
          readTwo NotElimination tryReadLn tryReadLn
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
        'e' -> do
          putStr "e"
          c3 <- getChar
          case c3 of
            'l' -> do
              putStrLn "e1"
              readTwo OrEliminationLeft tryReadLn tryReadLn
            'r' -> do
              putStrLn "e2"
              readTwo OrEliminationRight tryReadLn tryReadLn
            _ -> do
              putStrLn " | Error"
              return Nothing
        'i' -> do
          putStr "i"
          c3 <- getChar
          case c3 of
            'l' -> do
              putStrLn "(Number, Statement): "
              readTwo OrIntroductionLeft tryReadLn tryParseLn
            'r' -> do
              putStrLn "(Statement, Number): "
              readTwo OrIntroductionRight tryParseLn tryReadLn
            _ -> do
              putStrLn " | Error"
              return Nothing
        _ -> do
          putStrLn " | Error"
          return Nothing
    'p' -> do
      putStr "Premise: "
      readOne Premise tryParseLn
    'q' -> return $ Just Quit
    's' -> do
      putStr "Assumption: "
      readOne AssumptionIntroduction tryParseLn
    'S' -> do
      putStr "Search for: "
      readOne Search tryParseLn
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
  | OrIntroductionLeft Int Statement
  | OrIntroductionRight Statement Int
  | Premise Statement
  | Search Statement
  | Quit
  | FillerL
  deriving (Read, Show, Eq, Ord)

data Statement
  = And Statement Statement
  | Or Statement Statement
  | Not Statement
  | Implies Statement Statement
  | AssumptionBlock Statement [(Statement, Law)]
  | Variable String
  | Bottom
  | FillerS
  deriving (Read, Show, Eq, Ord)

convert :: Exp -> Statement
convert =
  \case
    EBtm -> Bottom
    EAnd l r -> And (convert l) (convert r)
    EOr l r -> Or (convert l) (convert r)
    EImpl l r -> Implies (convert l) (convert r)
    ENot e -> Not (convert e)
    EVar (Abs.Ident n) -> Variable n

parseStatement :: String -> Maybe Statement
parseStatement s =
  case Par.pExp . Par.myLexer $ s of
    Right res -> Just $ convert res
    Left _    -> Text.Read.readMaybe s

prettyShow :: Statement -> String
prettyShow = unwrap . prettyShow'
  where
    unwrap [] = []
    unwrap s
      | head s == '(' && last s == ')' = init . tail $ s
      | otherwise = s
    wrap s = "(" ++ s ++ ")"
    prettyShow' =
      \case
        l `And` r -> wrap $ prettyShow' l ++ " Λ " ++ prettyShow' r
        l `Or` r -> wrap $ prettyShow' l ++ " V " ++ prettyShow' r
        Not l -> "¬" ++ prettyShow' l
        l `Implies` r -> wrap $ prettyShow' l ++ " → " ++ prettyShow' r
        AssumptionBlock _ ss -> "[]"
        Variable c -> c
        Bottom -> "⊥"
        FillerS -> "(Ignore me)"

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
    ModusTollens x y -> "MT " ++ show x ++ ", " ++ show y
  -- NotIntroduction Int Int
  -- NotElimination Int Int
  -- NotBottom
  -- OrEliminationLeft Int Int
  -- OrEliminationRight Int Int
    OrIntroductionLeft x _ -> "Vi " ++ show x
    OrIntroductionRight _ x -> "Vi " ++ show x
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
    OrIntroductionLeft x y -> do
      x' <- ss !? x
      Just $ x' `Or` y
    OrIntroductionRight x y -> do
      y' <- ss !? y
      Just $ x `Or` y'
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
      Nothing
      -- error "hi"
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
    ModusTollens l r -> do
      l' <- ss !? l
      r' <- ss !? r
      case (l', r') of
        (x `Implies` y, Not y') ->
          if y == y'
            then Just $ Not x
            else Nothing
        (Not y', x `Implies` y) ->
          if y == y'
            then Just $ Not x
            else Nothing
        _ -> Nothing
    NotBottom -> return . Not . Not $ Bottom
    NotIntroduction l -> do
      case ss !? l of
        Just (x `Implies` Bottom) -> Just $ Not x
        _                         -> Nothing
    NotElimination _ _ -> do
      Nothing
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

------------------------------ SOLVING ------------------------------
substatements :: Statement -> S Statement
substatements s =
  Set.union (Set.singleton s) $
  case s of
    l `And` r           -> Set.union (substatements l) (substatements r)
    l `Or` r            -> Set.union (substatements l) (substatements r)
    l `Implies` r       -> Set.union (substatements l) (substatements r)
    Not l               -> substatements l
    AssumptionBlock _ _ -> Set.empty
    Variable n          -> Set.empty
    Bottom              -> Set.empty

-- Obviously a very rough heuristic
similarity :: Statement -> Statement -> Double
similarity l r =
  let lSet = substatements l
      rSet = substatements r
      all = Set.union lSet rSet
      shared = Set.intersection lSet rSet
   in fromIntegral (Set.size shared) / fromIntegral (Set.size all)

applicableRules :: [(Statement, Law)] -> [[(Statement, Law)]]
applicableRules ss =
  map (\s -> ss ++ [Maybe.fromJust s]) .
  filter Maybe.isJust . map (\l -> (, l) <$> checkStatement ss' l) $
  rules
  where
    ss' = map fst ss
    singles = [0 .. length ss]
    pairRules =
      [ AndIntroduction
      , Contradiction
      , NotElimination
      , ImplicationElimination
      , OrEliminationLeft
      , OrEliminationRight
      , ModusTollens
      ]
    singleRules =
      [ AndEliminationLeft
      , AndEliminationRight
      , DoubleNotIntroduction
      , DoubleNotElimination
      , ImplicationIntroduction
      , LEM
      , NotIntroduction
      ]
    singleRules' = [rule n | rule <- singleRules, n <- singles]
    pairRules' = [rule m n | rule <- pairRules, m <- singles, n <- singles]
    rules = singleRules' ++ pairRules'
  -- For later
  -- | OrIntroductionLeft Int Statement
  -- | OrIntroductionRight Statement Int
  -- | AssumptionIntroduction Statement
  -- | DeriveAnything Int Statement

bfs :: (Show a, Ord a) => (a -> Bool) -> (a -> [a]) -> [a] -> a
bfs _ _ [] = error "No solution found"
bfs done getNeighbours (start:queue)
  | done start = start
  | otherwise = bfs done getNeighbours (queue ++ newNeighbours)
  where
    queueS = Set.fromList queue
    newNeighbours =
      (queue ++) . filter (`Set.notMember` queueS) . getNeighbours $ start
