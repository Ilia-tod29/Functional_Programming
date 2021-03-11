main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not
    print $ truncatablePrime 37 == True
    print $ truncatablePrime 60 == False

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

truncatablePrime :: Int -> Bool
truncatablePrime num
 | num < 10 = isPrime num
 | not (isPrime num) = False
 | otherwise = truncatablePrime (div num 10)