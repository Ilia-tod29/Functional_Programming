main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11


reverseD :: Int -> Int -> Int 
reverseD num result 
 | num == 0 = result
 | otherwise = reverseD (div num 10) ((result * 10) + (mod num 10))
isPalindrome :: Int -> Bool
isPalindrome n = reverseD n 0 == n


countPalindromes :: Int -> Int -> Int
countPalindromes start end
 | start < 0 || end < 0 = error "Please insert positive numbers"
 | otherwise = if start >= end then helper (end + 1) start 0 else helper (start + 1) end 0
  where
      helper :: Int -> Int -> Int -> Int
      helper currentNum _end counter
       | currentNum == _end - 1 = counter
       | isPalindrome currentNum = helper (currentNum + 1) _end (counter + 1)
       | otherwise = helper (currentNum + 1) _end counter