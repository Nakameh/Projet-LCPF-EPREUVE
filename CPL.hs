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




findWorlds :: Formula -> [World]
findWorlds f = listGoodWorlds f (genAllWorlds (retireDoublons (extrait f)))



listGoodWorlds :: Formula -> [World] -> [World]
listGoodWorlds f [] = []
listGoodWorlds f (w:ws)
    | sat w f  = w : listGoodWorlds f ws
    | otherwise = listGoodWorlds f ws






extrait :: Formula -> [String]
extrait T = []
extrait F = []
extrait (And f1 f2) = extrait f1 ++ extrait f2
extrait (Or f1 f2) = extrait f1 ++ extrait f2
extrait (Imp f1 f2) = extrait f1 ++ extrait f2
extrait (Eqv f1 f2) = extrait f1 ++ extrait f2
extrait (Not f) = extrait f
extrait (Var x) = [x]


retireDoublons :: [String] -> [String]
retireDoublons [] = []
retireDoublons (x:xs)
    |contenu x xs = retireDoublons xs
    |otherwise = x : retireDoublons xs
    where
        contenu :: String -> [String] -> Bool
        contenu _ [] = False
        contenu x (y:ys)
            | x == y = True
            | otherwise = contenu x ys



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

testRetireDoublons :: [Bool]
testRetireDoublons = [
    null (retireDoublons []),
    retireDoublons ["p1"] == ["p1"],
    retireDoublons ["p1", "p2", "p1", "p2"] == ["p1", "p2"],
    retireDoublons ["p1", "p2", "p1", "p2", "p3", "p3"] == ["p1", "p2", "p3"]
    ]

testListGoodWorlds :: [Bool]
testListGoodWorlds = [
    listGoodWorlds (Var "p1") (genAllWorlds w0) == [["p1", "p2"], ["p1"]],
    listGoodWorlds (Var "t1") (genAllWorlds w1) == [["t1", "t2"], ["t1"]],
    listGoodWorlds (And (Var "p1") (Var "t2")) (genAllWorlds w2) == [["p1","p2","t1","t2"],["p1","p2","t2"],["p1","t1","t2"],["p1","t2"]],
    listGoodWorlds (And (Var "p1") (Not (Var "t1"))) (genAllWorlds w2) /= [["p1", "p2", "t1", "t2"], ["p1", "p2"]],
    listGoodWorlds (Not (Var "t1")) (genAllWorlds w1) /= [["t1", "t2"], ["t2"]]
    ]

testFindWorlds :: [Bool]
testFindWorlds = [
    findWorlds exemple == [["p1","p2","t1","t2"],["p1","p2","t1"],["p1","p2","t2"],["p1","t1","t2"],["p1","t1"],["p1","t2"],["p2","t1","t2"],["p2","t1"],["p2","t2"]],
    findWorlds (Var "p1") == [["p1"]],
    findWorlds (And (Var "p1") (Not (Var "t1"))) /= [["p1", "p2", "t1", "t2"], ["p1", "p2"]],
    findWorlds (Not (Var "t1")) /= [["t1", "t2"], ["t2"]]
    ]

test :: [Bool] -> Bool
test xs = foldr (&&) True xs

testAll :: [Char]
testAll
    |test testGenAllWorlds && test testSat && test testExtrait && test testRetireDoublons && test testListGoodWorlds && test testFindWorlds = "Success!"
    |otherwise = "Failure!"