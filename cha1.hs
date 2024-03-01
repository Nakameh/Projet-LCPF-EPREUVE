import CPL

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = And (Or(Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula
constraint = Not (Or (And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))

reglement :: Formula
reglement = Or (And door1 (Not door2)) (And door2 (Not door1))

challenge1 :: Formula
challenge1 = Or door1 door2


