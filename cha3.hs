import CPL

door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

door2 :: Formula
door2 = Var "p1"

constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))

reglement :: Formula
reglement = Or (And door1  door2) (And (Not door2) (Not door1))

challenge3 :: Formula
challenge3 = And constraint reglement



