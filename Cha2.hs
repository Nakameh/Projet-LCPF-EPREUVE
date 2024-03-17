-- Exportation de la fonction challenge2
module Cha2(
    challenge2
) where




-- Importation de l'objet Formula et des fonctions du module CPL
import CPL




-- |Fonction représentant la formule de la porte 1 du challenge 2
door1 :: Formula
door1 = Or (Var "p1") (Var "p2")




-- |Fonction représentant la formule de la porte 2 du challenge 2
door2 :: Formula
door2 = Var "t1"




-- |Fonction représentant la formule de la contrainte du challenge 2
constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))




-- |Fonction représentant la formule du réglement du challenge 2
reglement :: Formula
reglement = Or (And door1  door2) (And (Not door2) (Not door1))




-- |Fonction représentant la formule du challenge 2

challenge2 :: Formula
challenge2 = And constraint reglement
