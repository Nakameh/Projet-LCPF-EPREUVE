import CPL

door1 :: Formula
door1 = Var "t1"

door2 :: Formula
door2 = Var "p2"

door3 :: Formula
door3 = Var "t2"

singleElementByDoorConstraint :: Formula
singleElementByDoorConstraint = And (And (Or (And (Var "p1") (Not (Var "t1"))) (And (Var "t1") (Not (Var "p1")))) (Or (And (Var "p2") (Not (Var "t2"))) (And (Var "t2") (Not (Var "p2"))))) (Or (And (Var "p3") (Not (Var "t3"))) (And (Var "t3") (Not (Var "p3"))))

constraint :: Formula
constraint = Or (Or (And (And (Var "p1") (Var "t2")) (Var "t3")) (And (And (Var "p2") (Var "t1")) (Var "t3"))) (And (And (Var "p3") (Var "t1")) (Var "t2"))

reglement :: Formula
reglement = Or (Or (And door1 (And (Not door2) (Not door3))) (And door2 (And (Not door1) (Not door3)))) (And door3 (And (Not door1) (Not door2)))

challenge6 :: Formula
challenge6 = And (And singleElementByDoorConstraint constraint) reglement

main :: IO ()
main = do
    print (findWorlds challenge6)
