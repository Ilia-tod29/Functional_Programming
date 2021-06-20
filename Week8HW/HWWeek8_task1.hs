import Data.List
main :: IO()
main = do
    print $ allPaths 1 [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]  -- == [[1]]
    print $ allPaths 3 [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] --  == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]


nodes :: [(Int, [Int])] -> [Int]
nodes graph = [start | (start, _) <- graph]

successors :: Graph -> Node -> [Int]
successors [] _ = error "element not present"
successors ((parent, succ):xs) start
 | start == parent = succ
 | otherwise = successors xs start


allPaths :: Node -> Graph -> [Path]
allPaths start graph = nextLists
 where
     nextStarts = successors graph start
     nextLists
      | nextStarts == [] = [[start]]
      | otherwise = map ((:) start) $ concat $ map (\nextStart -> allPaths nextStart graph) nextStarts


simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph length' start 
 | not $ elem start (nodes graph) = error "The node is not present."
 | otherwise = filter (\ ys -> (length ys) == (length' + 1)) $ nub $ map (\ xs -> take (length' + 1) xs) (allPaths start graph)
