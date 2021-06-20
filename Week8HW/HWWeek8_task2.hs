import Data.List
main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

nodes :: [(Int, Int, Int)] -> [Int]
nodes xs = helper $ unzip3 xs
 where
     helper :: ([Int], [Int], [Int]) -> [Int]
     helper (xs, ys, zs) = nub $ xs ++ ys ++ zs

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves tree = helper tree (nodes tree)
 where
     helper :: [(Int, Int, Int)] -> [Int] -> [Int]
     helper ((x,_,_):xs) allNodes
      | xs == [] = filter (\ y -> y /= x) allNodes
      | otherwise = helper xs (filter (\ y -> y /= x) allNodes)