main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]
    print $ getParent t 10
    print $ getParent t 7
    print $ getGrantparent t 10
    print $ getGrantparent t 7
    print $ successors t 10
    print $ successors t 1

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

getParent :: Tree -> Int -> Int
getParent ts node = fst $ (filter (\ (par, ps) -> elem node ps) ts) !! 0

getGrantparent :: Tree -> Int -> (Int, [Int])
getGrantparent ts node = (filter (\ (par, ps) -> elem (getParent ts node) ps) ts) !! 0

findUncles :: Tree -> Int -> [Int]
findUncles ts node = filter (\ x -> x /= getParent ts node) (snd $ getGrantparent ts node)

successors :: [(Int, [Int])] -> Int -> [Int]
successors ((parent, succ):xs) start
 | start == parent = succ
 | otherwise = successors xs start