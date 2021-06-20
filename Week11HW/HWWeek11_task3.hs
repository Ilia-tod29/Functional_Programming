main :: IO()
main = do 
    -- When we work with levels we must start counting from zero... Soo shouldn't the answer be 2? If not just in the getfLevel and traverseBFSUpdate
    -- functions the code must be as in the comments :)
    print $ maxDepthBlueNode colorTree -- == 3
    print $ getLevel colorTree 3
    print $ reverse $ traverseBFSUpdate colorTree


data Color = Red | Green | Blue
 deriving (Show, Eq)
data Tree = Nil | Node Color Tree Tree


colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Nil Nil) Nil) (Node Red (Node Blue (Node Green Nil Nil) (Node Red Nil Nil)) Nil)
    

getLevel :: Tree -> Int -> [Color]
getLevel Nil _ = []
getLevel (Node value left right) k
 | k < 0 = [] -- k < 1 = []
 | k == 0 = [value] -- k == 1 = [value]
 | otherwise = getLevel left (k - 1) ++ getLevel right (k - 1)

traverseBFSUpdate :: Tree -> [(Int, Color)]
traverseBFSUpdate Nil = []
traverseBFSUpdate t = helper 0 -- helper 1
 where
     helper k
      | nodesAtLvlK == [] = []
      | otherwise = map (\ color -> (k, color)) nodesAtLvlK ++ helper (k + 1)
       where
           nodesAtLvlK = getLevel t k

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Nil = 0
maxDepthBlueNode tree
 | not $ any (\ (index, color) -> color == Blue) (traverseBFSUpdate tree) = 0
 | otherwise = fst $ take ((length upToFirstBlue) + 1) (reverse $ traverseBFSUpdate tree) !! (length upToFirstBlue)
  where
      upToFirstBlue = takeWhile (\ (index, color) -> color /= Blue) (reverse $ traverseBFSUpdate tree)