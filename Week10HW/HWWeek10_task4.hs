main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ filteredTree t1
    print $ filteredTree t2
data NTree = Nil | Node Int [NTree]
 deriving (Show)

t1 :: NTree
t1 = Node 1 [(Node 3 [Nil]), (Node 5 [Nil]), (Node 7 [Nil]), (Node 9 [Nil])]

t2 :: NTree
t2 = Node 1 [(Node 7 [Nil]), (Node 5 [Nil]), (Node 2 [Nil])]

filteredTree :: NTree -> [NTree]
filteredTree (Node val rs) = filter (\ (Node v ls) -> mod (abs (v - val)) 2 == 0) rs

isGraceful :: NTree -> Bool
isGraceful (Node _ [Nil]) = True
isGraceful tree@(Node val rs)
 | (length $ filteredTree tree) /= length rs = False
 | otherwise = all (== True) (map (\ x -> isGraceful x) rs)