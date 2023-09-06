import Data.Vect

main : IO ()
main = putStrLn "Hello"

data Law : Nat -> Type where
  AndEliminationLeft : {n: Nat} -> Fin n -> Law n
  AndEliminationRight : {n: Nat} -> Fin n -> Law n
  AndIntroduction : {n: Nat} -> Fin n -> Fin n -> Law n

  OrEliminationLeft : {n: Nat} -> Fin n -> Fin n -> Law n
  OrEliminationRight : {n: Nat} -> Fin n -> Fin n -> Law n
  OrIntroduction : {n: Nat} -> Fin n -> Fin n -> Law n

  ImplicationElimination : {n: Nat} -> Fin n -> Fin n -> Law n
  ImplicationIntroduction : {n: Nat} -> Fin n -> Fin n -> Law n
  
  DoubleNotIntroduction : {n: Nat} -> Fin n -> Law n
  DoubleNotElimination : {n: Nat} -> Fin n -> Law n

data Statement
  = And Statement Statement
  | Or Statement Statement
  | Not Statement
  | Implies Statement Statement
  | AssumptionBlock Statement (List Statement)

partial -- Because I can't find anything on explicit Eliminators in the book
Eq Statement where
  (And l r) == (And l' r') =
    l == l' && r == r'
  (Or l r) == (Or l' r') =
    l == l' && r == r'
  (Not x) == (Not x') =
    x == x'
  (Implies l r) == (Implies l' r') =
    l == l' && r == r'
  (AssumptionBlock p ss) == (AssumptionBlock p' ss') =
    p == p' && ss == ss'
  _ == _ = False

last : List a -> Maybe a
last [] = Nothing
last [x] = Just x
last (_::x::xs) = last (x::xs)

checkStatement : {n: Nat} -> Vect n Statement -> Law n -> Maybe Statement
checkStatement ss (AndEliminationLeft x) =
  case index x ss of
    l `And` _ => Just l
    _ => Nothing
checkStatement ss (AndEliminationRight x) =
  case index x ss of
    _ `And` r => Just r
    _ => Nothing
checkStatement ss (AndIntroduction x y) =
  Just $ (index x ss) `And` (index y ss)
checkStatement ss (ImplicationIntroduction x y) =
  case index x ss of
    AssumptionBlock premise zz =>
      let last' = case last zz of
            Just x => x
            Nothing => premise
        in Just $ premise `Implies` last'
    _ => Nothing
checkStatement ss (DoubleNotIntroduction x) = Just . Not . Not $ index x ss
checkStatement ss (DoubleNotElimination x) =
  case index x ss of
    Not (Not x) => Just x
    _ => Nothing
checkStatement ss (OrEliminationLeft x y) =
  case (index x ss, index y ss) of
    (l `Or` _, l') =>
      if l == l'
        then Just l
        else Nothing
    _ => Nothing
checkStatement ss (OrEliminationRight x y) =
  case (index x ss, index y ss) of
    (_ `Or` r, r') =>
      if r == r'
        then Just r
        else Nothing
    _ => Nothing
checkStatement _ _ = ?checkStatement_rhs

check : {n: Nat} -> Vect n Statement -> Bool
