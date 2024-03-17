-- Exportation de la fonction challenge6
module Cha6(
    challenge6
) where




-- Importation de l'objet Formula et des fonctions du module CPL
import CPL




-- |Fonction représentant la formule de la porte 1 du challenge 6
door1 :: Formula
door1 = Var "t1"




-- |Fonction représentant la formule de la porte 2 du challenge 6
door2 :: Formula
door2 = Var "p2"




-- |Fonction représentant la formule de la porte 3 du challenge 6
door3 :: Formula
door3 = Var "t2"




-- |Fonction représentant la contrainte qui dit qu'il ne peut pas y avoir
--  un tiger et une peluche dans la même cellule du challenge 6
singleElementByDoorConstraint :: Formula
singleElementByDoorConstraint = And (And (Or (And (Var "p1") (Not (Var "t1"))) (And (Var "t1") (Not (Var "p1")))) (Or (And (Var "p2") (Not (Var "t2"))) (And (Var "t2") (Not (Var "p2"))))) (Or (And (Var "p3") (Not (Var "t3"))) (And (Var "t3") (Not (Var "p3"))))




-- |Fonction représentant la contrainte qui dit qu'il n'y a qu'une
--  seule peluche puis que des tigres du challenge 6
constraint :: Formula
constraint = Or (Or (And (And (Var "p1") (Var "t2")) (Var "t3")) (And (And (Var "p2") (Var "t1")) (Var "t3"))) (And (And (Var "p3") (Var "t1")) (Var "t2"))




-- |Fonction représentant la formule du réglement du challenge 6
reglement :: Formula
reglement = Or (Or (And door1 (And (Not door2) (Not door3))) (And door2 (And (Not door1) (Not door3)))) (And door3 (And (Not door1) (Not door2)))




-- |Fonction représentant la formule du challenge 6
challenge6 :: Formula
challenge6 = And (And singleElementByDoorConstraint constraint) reglement
