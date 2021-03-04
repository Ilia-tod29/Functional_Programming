main :: IO()
main = do
    print $ myGcd 5 13 == 1
    print $ myGcd 13 1235 == 13

myGcd :: Int -> Int -> Int
myGcd x y 
 | x == 0 = abs y
 | y == 0 = abs x
 | x < y = myGcd y x
 | otherwise = myGcd y (mod x y)