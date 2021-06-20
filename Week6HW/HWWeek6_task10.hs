import Data.Char
main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)



checkNumber :: Int -> (Int, Int)
checkNumber num = (foldr1 (+) (map (digitToInt) $ fst $ unzip $ filter (\ (_,x) -> odd x) $ zip (show num) [1 ..]), foldr1 (+) (map (digitToInt) $ fst $ unzip $ filter (\ (_,x) -> even x) $ zip (show num) [1 ..]))

-- Solution made before reading that ith should be on one line with folding :)
-- checkNumber :: Int -> (Int, Int)
-- checkNumber num = (oddSum num, evenSum num)
--  where
--      evenSum :: Int -> Int
--      evenSum n = sum $ map (digitToInt) $ fst $ unzip $ filter (\ (_,x) -> even x) $ zip (show n) [1 ..]

--      oddSum :: Int -> Int
--      oddSum n = sum $ map (digitToInt) $ fst $ unzip $ filter (\ (_,x) -> odd x) $ zip (show n) [1 ..]