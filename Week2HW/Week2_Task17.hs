main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

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

isThereADigit :: Int -> Int -> Bool
isThereADigit num digit 
 | num == 0 = False
 | mod num 10 == digit = True
 | otherwise = isThereADigit (div num 10) digit

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d
 | n <= 0 || d <= 0 = error "Please enter positive numbers"
 | otherwise = helper 0 2 0
  where
      helper :: Int -> Int -> Int -> Int 
      helper counter num result
       | counter == n = result
       | isPrime num && isThereADigit num d = helper (counter + 1) (num + 1) (result + num)
       | otherwise = helper counter (num + 1) result