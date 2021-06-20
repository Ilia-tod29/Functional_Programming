main :: IO()
main = do
    print $ isArithmentic [3] == True
    print $ isArithmentic [3, 5] == True
    print $ isArithmentic [1, 2, 3, 4, 5] == True
    print $ isArithmentic [3, 5, 7, 9, 11] == True
    print $ isArithmentic [3, 5, 8, 9, 11] == False



isArithmentic :: [Int] -> Bool
isArithmentic [] = False
isArithmentic [_] = True
isArithmentic [_, _] = True
isArithmentic (x:xs) = (head xs) == (xs !! 1) - ((head xs) - x) && isArithmentic xs

--Leaving it here for me :)

-- isArithmentic :: [Int] -> Bool
-- isArithmentic [] = False
-- isArithmentic (x:xs)
--  | xs == [] = True
--  | otherwise = helper (head xs - x) 0 xs
--   where
--       helper :: Int -> Int -> [Int] -> Bool
--       helper diff index list
--        | index == (length list) - 1 = True
--        | list !! index /= (list !! (index + 1)) - diff = False
--        | otherwise = helper diff (index + 1) list