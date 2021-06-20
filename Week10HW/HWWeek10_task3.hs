import Data.List
main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data Tree = Nil | Node (Int, Int) Tree Tree
 deriving (Show, Eq)

t1 :: Tree
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: Tree
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

traverseDFS :: Tree -> [Int]
traverseDFS Nil = []
traverseDFS (Node value left right) = (traverseDFS left) ++ [(snd value) - (fst value)] ++ (traverseDFS right)

ordered :: Tree-> Bool
ordered Nil = True
ordered t = traverseDFS t == (sort $ traverseDFS t)