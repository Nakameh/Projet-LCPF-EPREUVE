import CPL

door1 :: Formula
door1 = Or (And (Var "p1") (Var "t2") ) (And (Var "p2") (Var "t1"))

door2 :: Formula
door2 = Var "p1"

constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))

reglement :: Formula
reglement = And (Or (And (Var "p1") door1) (And (Var "t1") (Not door1))) (Or (And (Var "t2") door2) (And (Var "p2") (Not door2)))

challenge5 :: Formula
challenge5 = And constraint reglement


main :: IO ()
main = do
    print (findWorlds challenge5)
