main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0


data Shape a = Circle a | Rectangle a a | Cylinder a a | Triangle a a a
 deriving (Show, Eq)

area :: (Num a, Floating a) => (Shape a) -> a
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r *r
area (Triangle a b c) = sqrt(p * (p - a) * (p - b) * (p - c))
 where
     p = (a + b + c) / 2

getAreas :: (Num a, Floating a) => [(Shape a)] -> [a]
getAreas xs = map (\ sh -> area sh) xs

maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
maxArea xs = foldr1 (\ x y -> if (area x) > (area y) then x else y) xs


-- findIndex :: [Shape] -> Int
-- findIndex xs = helper (maximum $ getAreas xs) (getAreas xs) 0
--  where 
--      helper :: Double -> [Double] -> Int -> Int
--      helper max lst index
--       | max == lst !! index = index
--       | otherwise = helper max lst (index + 1)

-- maxArea :: [Shape] -> Shape
-- maxArea xs = xs !! (findIndex xs)


