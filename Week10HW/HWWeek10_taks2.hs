main :: IO()
main = do 
    -- When we work with levels we must start counting from zero... Soo shouldn't the answer be 2? If not just in the getfLevel function the code must be as in the comments :)
    print $ minDepthGreenNode colorTree -- == 3
    print $ minDepthGreenNode Nil -- == 3
    print $ getLevel colorTree 3


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


minDepthGreenNode :: Tree -> Int
minDepthGreenNode Nil = 0
minDepthGreenNode tree = helper 0
 where
     helper :: Int -> Int
     helper lvl
      | any (\ color -> color == Green) (getLevel tree lvl) = lvl
      | otherwise = helper (lvl + 1)