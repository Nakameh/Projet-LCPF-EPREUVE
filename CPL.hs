module CPL (
    Formula (..),
    World,
    genAllWorlds,
    testGenAllWorlds,
    sat,
    testSat,
    findWorlds,
    testFindWorlds,
    testAll
) where



-- |Type de données pour les formules logiques
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




-- |Fonction qui renvoie un exemple de formule possible
exemple :: Formula
exemple = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))




-- |Type de données pour les mondes représentés par des listes de chaînes de caractères
type World = [String]




-- |Fonction qui renvoie un monde possible contenant uniquement des peluches
w0 :: World
w0 = ["p1", "p2"]




-- |Fonction qui renvoie un monde possible contenant des tigres
w1 :: World
w1 = ["t1", "t2"]




-- |Fonction qui renvoie un monde possible contenant des peluches et des tigres
w2 :: World
w2 = ["p1", "p2", "t1", "t2"]




-- |Fonction qui génère tous les mondes possibles à partir d'une liste de chaînes de caractères
genAllWorlds :: [String] -> [World]
genAllWorlds [] = [[]]
genAllWorlds (x:xs) = map (x:) gaw ++ gaw
    where gaw = genAllWorlds xs




-- |Fonction qui renvoie True si une formule est vraie dans un monde donné
sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (And f1 f2) = sat w f1 && sat w f2
sat w (Or f1 f2) = sat w f1 || sat w f2
sat w (Imp f1 f2) = not (sat w f1) || sat w f2
sat w (Eqv f1 f2) = sat w f1 == sat w f2
sat w (Not f) = not (sat w f)
sat w (Var str) = contient str w
    where
        contient _ [] = False
        contient x (y:ys)
            | x == y = True
            | otherwise = contient x ys




-- |Fonction qui renvoie tous les mondes possibles dans lesquels une formule est vraie
findWorlds :: Formula -> [World]
findWorlds f = listGoodWorlds f (genAllWorlds (retireDoublons (extrait f)))




-- |Fonction qui renvoie tous les mondes donnés possible dans lesquels une formule donnée est vraie
listGoodWorlds :: Formula -> [World] -> [World]
listGoodWorlds _ [] = []
listGoodWorlds f (w:ws)
    | sat w f  = w : listGoodWorlds f ws
    | otherwise = listGoodWorlds f ws




-- |Fonction qui renvoie les variables d'une formule
extrait :: Formula -> [String]
extrait T = []
extrait F = []
extrait (And f1 f2) = extrait f1 ++ extrait f2
extrait (Or f1 f2) = extrait f1 ++ extrait f2
extrait (Imp f1 f2) = extrait f1 ++ extrait f2
extrait (Eqv f1 f2) = extrait f1 ++ extrait f2
extrait (Not f) = extrait f
extrait (Var x) = [x]




-- |Fonction qui retire les doublons d'une liste de chaînes de caractères
retireDoublons :: [String] -> [String]
retireDoublons [] = []
retireDoublons (x:xs)
    |contenu x xs = retireDoublons xs
    |otherwise = x : retireDoublons xs
    where
        contenu :: String -> [String] -> Bool
        contenu _ [] = False
        contenu str (y:ys)
            | str == y = True
            | otherwise = contenu str ys




-- |Fonction qui teste la fonction genAllWorlds
testGenAllWorlds :: [Bool]
testGenAllWorlds = [
    genAllWorlds [] == [[]],
    genAllWorlds ["p1"] == [["p1"], []],
    genAllWorlds w0== [["p1", "p2"], ["p1"], ["p2"], []],
    genAllWorlds w2 == [
        ["p1", "p2", "t1", "t2"],
        ["p1", "p2", "t1"],
        ["p1", "p2", "t2"],
        ["p1", "p2"],
        ["p1", "t1", "t2"],
        ["p1", "t1"],
        ["p1", "t2"],
        ["p1"],
        ["p2", "t1", "t2"],
        ["p2", "t1"],
        ["p2", "t2"],
        ["p2"],
        ["t1", "t2"],
        ["t1"],
        ["t2"],
        []
        ]
    ]




-- |Fonction qui teste la fonction sat
testSat :: [Bool]
testSat = [
    sat w0 (Var "p2"),
    not (sat w0 (Var "t1")),
    not (sat w0 (Var "t2")),
    sat w0 (And (Var "p1") (Var "p2")),
    sat w1 (Or (Var "t1") (Var "t2")),
    sat w2 (And (Var "p1") (Var "t2")),
    not (sat w2 (And (Var "p1") (Not (Var "t1")))),
    not (sat w1 (Not (Var "t1")))
    ]




-- |Fonction qui teste la fonction extrait
testExtrait :: [Bool]
testExtrait = [
    null (extrait T),
    null (extrait F),
    extrait (And (Var "p1") (Var "p2")) == ["p1", "p2"],
    extrait (Or (Var "p1") (Var "p2")) == ["p1", "p2"],
    extrait (Imp (Var "p1") (Var "p2")) == ["p1", "p2"],
    extrait (Eqv (Var "p1") (Var "p2")) == ["p1", "p2"],
    extrait (Not (Var "p1")) == ["p1"],
    extrait (Var "p1") == ["p1"]
    ]




-- |Fonction qui teste la fonction retireDoublons
testRetireDoublons :: [Bool]
testRetireDoublons = [
    null (retireDoublons []),
    retireDoublons ["p1"] == ["p1"],
    retireDoublons ["p1", "p2", "p1", "p2"] == ["p1", "p2"],
    retireDoublons ["p1", "p2", "p1", "p2", "p3", "p3"] == ["p1", "p2", "p3"]
    ]




-- |Fonction qui teste la fonction listGoodWorlds
testListGoodWorlds :: [Bool]
testListGoodWorlds = [
    listGoodWorlds (Var "p1") (genAllWorlds w0) == [["p1", "p2"], ["p1"]],
    listGoodWorlds (Var "t1") (genAllWorlds w1) == [["t1", "t2"], ["t1"]],
    listGoodWorlds (And (Var "p1") (Var "t2")) (genAllWorlds w2) == [["p1","p2","t1","t2"],["p1","p2","t2"],["p1","t1","t2"],["p1","t2"]],
    listGoodWorlds (And (Var "p1") (Not (Var "t1"))) (genAllWorlds w2) /= [["p1", "p2", "t1", "t2"], ["p1", "p2"]],
    listGoodWorlds (Not (Var "t1")) (genAllWorlds w1) /= [["t1", "t2"], ["t2"]]
    ]




-- |Fonction qui teste la fonction findWorlds
testFindWorlds :: [Bool]
testFindWorlds = [
    findWorlds exemple == [
                            ["p1","p2","t1","t2"],
                            ["p1","p2","t1"],
                            ["p1","p2","t2"],
                            ["p1","t1","t2"],
                            ["p1","t1"],
                            ["p1","t2"],
                            ["p2","t1","t2"],
                            ["p2","t1"],
                            ["p2","t2"]
                        ],
    findWorlds (Var "p1") == [["p1"]],
    findWorlds (And (Var "p1") (Not (Var "t1"))) /= [["p1", "p2", "t1", "t2"], ["p1", "p2"]],
    findWorlds (Not (Var "t1")) /= [["t1", "t2"], ["t2"]]
    ]




-- | Fonction qui vérifie que tout les booléens d'une liste sont vrais
test :: [Bool] -> Bool
test xs = foldr (&&) True xs




-- | Fonction qui réalise tous les tests et renvoie "Success!" si ils sont tous vrais, "Failure!" sinon
testAll :: [Char]
testAll
    |test testGenAllWorlds && test testSat && test testExtrait && test testRetireDoublons && test testListGoodWorlds && test testFindWorlds = "Success!"
    |otherwise = "Failure!"
