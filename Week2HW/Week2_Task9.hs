main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0


reverseD :: Int -> Int -> Int 
reverseD num result 
 | num == 0 = result
 | otherwise = reverseD (div num 10) ((result * 10) + (mod num 10))

removeD :: Int -> Int -> Int
removeD d num 
 | d < 0 = error "Please insert positive digit"
 | otherwise = helper (div num 10) (mod num 10) 0
  where
      helper :: Int -> Int -> Int -> Int
      helper thisNum digit result
       | thisNum == 0 && digit /= d = reverseD ((result * 10) + digit) 0
       | digit == d = helper (div thisNum 10) (mod thisNum 10) result
       | otherwise = helper (div thisNum 10) (mod thisNum 10) ((result * 10) + digit)