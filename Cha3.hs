-- Exportation de la fonction challenge3
module Cha3(
    challenge3
) where




-- Importation de l'objet Formula et des fonctions du module CPL
import CPL




-- |Fonction représentant la formule de la porte 1 du challenge 3
door1 :: Formula
door1 = Or (Var "t1") (Var "p2")




-- |Fonction représentant la formule de la porte 2 du challenge 3
door2 :: Formula
door2 = Var "p1"




-- |Fonction représentant la formule de la contrainte du challenge 3
constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))




-- |Fonction représentant la formule du réglement du challenge 3
reglement :: Formula
reglement = Or (And door1  door2) (And (Not door2) (Not door1))




-- |Fonction représentant la formule du challenge 3
challenge3 :: Formula
challenge3 = And constraint reglement
