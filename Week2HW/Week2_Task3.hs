main :: IO()
main = do
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 45136 == 53
    print $ sumPrimeDivs 7 == 7
    print $ sumPrimeDivs 1 == 0
    
    


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

sumPrimeDivs :: Int -> Int
sumPrimeDivs 1 = 0
sumPrimeDivs num 
 | isPrime (num) = num
 | otherwise = helper 2 0
    where
        helper :: Int -> Int -> Int
        helper currentDivisor result
         | currentDivisor == div num 2 && isPrime (currentDivisor) = result + currentDivisor
         | currentDivisor == div num 2 = result
         | mod num currentDivisor == 0 && isPrime (currentDivisor) = helper (currentDivisor + 1) (result + currentDivisor)
         | otherwise = helper (currentDivisor + 1) (result)
