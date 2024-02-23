data Formula a =    T | F | And (Formula a) (Formula a) | Or (Formula a) (Formula a) |
                    Imp (Formula a) (Formula a) | Eqv (Formula a) (Formula a) |
                    Not (Formula a) | Var String deriving (Show)


example =
    And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

