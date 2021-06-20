import Data.List
main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

deleteAt :: Int -> [Int] -> [Int]
deleteAt idx xs = left ++ right
  where (left, (_:right)) = splitAt idx xs


filterList :: [Int] -> Int -> Bool -> [Int]
filterList xs index flag
 | length xs - 1 <= index && not flag = xs
 | length xs == 1 && flag = filterList (deleteAt index xs) index False
 | xs !! index == xs !! (index + 1) = filterList (deleteAt (index + 1) xs) index True
 | flag = filterList (deleteAt index xs) index False
 | otherwise = filterList xs (index + 1) False

sumUnique :: [[Int]] -> Int
sumUnique xs = sum $ map sum $ map (\ x -> filterList (sort x) 0 False) xs 