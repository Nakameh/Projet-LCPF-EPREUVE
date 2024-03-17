-- Exportation de la fonction challenge1
module Cha1(
    challenge1
) where

-- Importation de l'objet Formula et des fonctions du module CPL
import CPL




-- |Fonction représentant la formule de la porte 1 du challenge 1
door1 :: Formula
door1 = And (Var "p1") (Var "t2")




-- |Fonction représentant la formule de la porte 2 du challenge 1
door2 :: Formula
door2 = And (Or(Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))




-- |Fonction représentant la formule de contrainte du challenge 1
constraint :: Formula
constraint = And (Not(Or(And (Var "p1") (Var "t1")) (And (Var "p2") (Var "t2")))) (And (Or (Var "p1") (Var "t1")) (Or (Var "p2") (Var "t2")))




-- |Fonction représentant la formule de règlement du challenge 1
reglement :: Formula
reglement = Or (And door1 (Not door2)) (And door2 (Not door1))




-- |Fonction représentant la formule du challenge 1
challenge1 :: Formula
challenge1 = And constraint reglement
