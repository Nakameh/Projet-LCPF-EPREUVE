-- Exportation de la fonction challenge5
module Cha5(
    challenge5
) where




-- Importation de l'objet Formula et de la fonction findWorlds
import CPL





-- |Fonction représentant la formule de la porte 1 du challenge 5
door1 :: Formula
door1 = Or (And (Var "p1") (Var "t2") ) (And (Var "p2") (Var "t1"))





-- |Fonction représentant la formule de la porte 2 du challenge 5
door2 :: Formula
door2 = Var "p1"





-- |Fonction représentant la formule de la contrainte du challenge 5
constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))





-- |Fonction représentant la formule du réglement du challenge 5
reglement :: Formula
reglement = And (Or (And (Var "p1") door1) (And (Var "t1") (Not door1))) (Or (And (Var "t2") door2) (And (Var "p2") (Not door2)))




-- |Fonction représentant la formule du challenge 5
challenge5 :: Formula
challenge5 = And constraint reglement
