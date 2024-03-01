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

genAllWorlds :: [String] -> [World]
genAllWorlds [] = [[]]
genAllWorlds (x:xs) = map (x:) gaw ++ gaw
    where gaw = genAllWorlds xs

sat :: World -> Formula -> Bool
sat w T = True
sat w F = False
sat w (And f1 f2) = sat w f1 && sat w f2
sat w (Or f1 f2) = sat w f1 || sat w f2
sat w (Imp f1 f2) = not (sat w f1) || sat w f2
sat w (Eqv f1 f2) = sat w f1 == sat w f2
sat w (Not f) = not (sat w f)
sat w (Var x) = contient w x
    where 
        contient [] _ = False
        contient (y:ys) x 
            | x == y = True
            | otherwise = contient ys x
