module CPL (
  Formula (..),
  World
) where 

data Formula
  = T
  | F
  | And Formula Formula
  | Or Formula Formula
  | Imp Formula Formula
  | Eqv Formula Formula
  | Not Formula
  | Var String
  deriving (Show)

exemple :: Formula
exemple = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

type World = [String]

w0 :: World
w0 = ["p1", "p2"]

w1 :: World
w1 = ["t1", "t2"]

w2 :: World
w2 = ["p1", "p2", "t1", "t2"]