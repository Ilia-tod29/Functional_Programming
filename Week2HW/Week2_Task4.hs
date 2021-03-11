main :: IO()
main = do
    print $ isPerfect 1 == False -- the sum of the divisors is 0, because of the hint
    print $ isPerfect 6 == True -- 1 + 2 + 3 = 6 = 6
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True


sumDivs :: Int -> Int
sumDivs n 
 | n < 0 = error "n was negative"
 | otherwise = helper 1
  where
      helper :: Int -> Int
      helper currentDivisor 
       | currentDivisor >= n = n
       | mod n currentDivisor == 0 = currentDivisor + helper (currentDivisor + 1)
       | otherwise = helper (currentDivisor + 1) 

isPerfect :: Int -> Bool
isPerfect 1 = False
isPerfect num
 | sumDivs num == num = True
 | otherwise = False