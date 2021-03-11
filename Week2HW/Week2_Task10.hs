main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther num
 | num < 10 = error "Please insert a numberlarger than 10"
 | otherwise = helper (div num 100) (mod (div num 10) 10) 0
  where
      helper :: Int -> Int -> Int -> Int 
      helper thisNum digit result
       | thisNum == 0 && digit /= 0 = ((result * 10) + digit)
       | thisNum == 0 = result
       | otherwise = helper (div thisNum 100) (mod (div thisNum 10) 10) ((result * 10) + digit)