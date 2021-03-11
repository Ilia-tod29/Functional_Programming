main :: IO()
main = do
    print $ isSpecial 131 2 == True
    print $ isSpecial 472 2 == False
    print $ isSpecial 17197 2 == True
    print $ isSpecial 12234 3 == False
    print $ isSpecial 10113 3 == True
    print $ isSpecial 353 2 == False

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n
 | n < 1 = error "n was negative"
 | otherwise = helper 2
  where
      helper :: Int -> Bool
      helper currentDivisor
       | currentDivisor == n = True
       | mod n currentDivisor == 0 = False
       | otherwise = helper (currentDivisor + 1)
     

isSpecial :: Int -> Int -> Bool
isSpecial n k
 | n <= 0 || k <= 0 = error "Please enter positive numbers"
 | otherwise = helper n (mod n (10 ^ k))
  where
      helper :: Int -> Int -> Bool
      helper thisN currentNumber
       | thisN < 10 ^ k && isPrime currentNumber = True
       | thisN < 10 ^ k = False 
       | not (isPrime currentNumber) = False
       | otherwise = helper (div thisN 10) (mod (div thisN 10) (10 ^ k))