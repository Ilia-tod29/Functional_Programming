main :: IO()
main = do
    print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
    print $ mySin 100 0.5 == 0.479425538604203

isEven :: Int -> Bool
isEven x = if (mod x 2) == 0 then True else False

fact :: Int -> Double
fact x
 | x == 0 = 1
 | otherwise = (fromIntegral x) * fact (x - 1)

mySin :: Int -> Double -> Double
mySin n x
 | n < 0 = error "n must be positive"
 | otherwise = helper 0 1 0.0
  where
      helper :: Int -> Int -> Double -> Double
      helper counter power result
       | counter == n + 1 = result
       | otherwise = if isEven counter then helper (counter + 1) (power + 2) (result + (x ^ power) / fact power)
                                        else helper (counter + 1) (power + 2) (result - (x ^ power) / fact power) 