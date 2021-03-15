main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764


isEven :: Int -> Bool
isEven x = if (mod x 2) == 0 then True else False

--This func is adjusted to the task a.k.a. it returns 2^(n+1)
pow2 :: Int -> Double
pow2 n
 | n == 0 = 2
 | otherwise = 2 * pow2 (n - 1)

calcSeriesSum :: Double -> Int -> Double
calcSeriesSum x n
 | n < 0 = error "n was negative"
 | otherwise = helper 0 3 2 1.0 0.0
  where
      helper :: Int -> Int -> Double -> Double -> Double -> Double
      helper counter multipyDenominator currentCollectible denominator result
       | counter == n = if isEven counter then result - currentCollectible else result + currentCollectible 
       | otherwise = if isEven counter then helper (counter + 1) (multipyDenominator + 2) (((pow2 (counter + 1)) * (x ^ (counter + 1))) / (denominator * (fromIntegral multipyDenominator))) (denominator * (fromIntegral multipyDenominator)) (result - currentCollectible)
                                        else helper (counter + 1) (multipyDenominator + 2) (((pow2 (counter + 1)) * (x ^ (counter + 1))) / (denominator * (fromIntegral multipyDenominator))) (denominator * (fromIntegral multipyDenominator)) (result + currentCollectible)
                    


                    -- then helper (counter + 1) (multipyDenominator + 2) (((pow2 counter) * (x ^ counter)) / (denominator * (fromIntegral multipyDenominator))) (denominator * (fromIntegral multipyDenominator)) (result - currentCollectible)
                    
                    -- else helper (counter + 1) (multipyDenominator + 2) (((pow2 counter) * (x ^ counter)) / (denominator * (fromIntegral multipyDenominator))) (denominator * (fromIntegral multipyDenominator)) (result + currentCollectible)