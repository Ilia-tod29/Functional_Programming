import Data.List 
main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364


getPalindromesList :: Int -> [Int]
getPalindromesList n = (filter (\ x -> mod n x == 0 && x == read (reverse (show x))) [2, 3 .. n])

getPalindromes :: Int -> Int
getPalindromes n = minimum (getPalindromesList n) + maximum (getPalindromesList n)