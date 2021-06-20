import Data.List
import Data.Char
main :: IO()
main = do
    print $ duplicateCount "" == 0 -- no characters repeats more than once
    print $ duplicateCount "aaaAAAAaaaaa" == 1
    print $ duplicateCount "abcde" == 0
    print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
    print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
    print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
    print $ duplicateCount "Indivisibility" == 1
    print $ duplicateCount "aA11" == 2 -- 'a' and '1'
    print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
    print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
    print $ duplicateCount ['a'..'z'] == 0
    print $ duplicateCount (['a'..'z'] ++ ['A'..'Z']) == 26


deleteAt :: Int -> String -> String
deleteAt idx xs = left ++ right
  where (left, (_:right)) = splitAt idx xs

duplicateCount :: String -> Int
duplicateCount str = filterList (sort $ map toLower str) 0 False 0
 where
     -- Almost the same logic like int task 7
     filterList :: String -> Int -> Bool -> Int -> Int
     filterList xs index flag counter
      | length xs - 1 <= index && not flag = counter
      | length xs == 1 && flag = filterList (deleteAt index xs) index False (counter + 1)
      | xs !! index == xs !! (index + 1) = filterList (deleteAt (index + 1) xs) index True counter| flag = filterList (deleteAt index xs) index False (counter + 1)
      | otherwise = filterList xs (index + 1) False counter