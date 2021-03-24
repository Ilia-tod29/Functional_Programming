main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6
    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6
    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

mySumRecNonPM :: [Double] -> Double
mySumRecNonPM xs = sum xs

mySumRecPM :: [Double] -> Double
mySumRecPM [] = 0
mySumRecPM xs = sum xs

mySumFunc :: [Double] -> Double
mySumFunc xs = sum $ (map (\ x -> x) xs)