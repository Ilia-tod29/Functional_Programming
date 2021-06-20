main :: IO()
main = do
    print $ calcExpression [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998
    print $ (myPoly [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998
    print $ calcExpression [] 2.2 3 

-- As the y must be an index I assume that it should be 'Int'
calcExpression :: [Double] -> Double -> Int -> Double
calcExpression [] _ _= 0
calcExpression ls x y = foldr1 (*) (map (\ p -> x - p) $ take y ls)

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly ls = (\ x y -> foldr1 (*) (map (\ p -> x - p) $ take y ls))