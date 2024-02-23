data Formula a
  = T
  | F
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Eqv (Formula a) (Formula a)
  | Not (Formula a)
  | Var a
  deriving (Show)


--La formule And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))
-- décrit une situation où il y a une porte qui peut contenir sois la peluche 1 ou la peluche 2
-- et une autre porte qui peut contenir soit le tigre 1 ou le tigre 2

exemple :: Formula String
exemple =
  And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula String
constraint = Not (And (Var "t1") (Var "t2"))

reglement :: Formula String
reglement = Or (And (Var "p1") (Not (Var "p2"))) (And (Not (Var "p1")) (Var "p2"))


