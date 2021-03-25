main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM num xs 
 | xs == [] = False
 | head xs == num = True
 | otherwise = isPresentRecNonPM num (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM num (x:xs)
 | x == num = True
 | otherwise = isPresentRecPM num xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc num xs = elem num xs

-- HOF
-- isPresentFunc :: Int -> [Int] -> Bool
-- isPresentFunc num xs = any (==num) xs
