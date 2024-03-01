import CPL

door1 :: Formula
door1 = Or (Var "p1") (Var "p2")

door2 :: Formula
door2 = Var "t1"

constraint :: Formula
constraint = Not (Or (And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))

reglement :: Formula
reglement = Or (And door1  door2) (And (Not door2) (Not door1))

challenge2 :: Formula
challenge2 = And constraint reglement


