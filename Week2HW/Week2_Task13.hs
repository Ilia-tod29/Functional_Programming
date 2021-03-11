main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98


findSum :: Int -> Int -> Int -> Int
findSum a b n
 | n <= 3 = error "n must ne higher than 3"
 | otherwise = genAndCalcSequence (n - 1) 0 0
  where
      genAndCalcSequence :: Int -> Int -> Int -> Int
      genAndCalcSequence power counter result
       | counter == 3 = result
       | otherwise = genAndCalcSequence (power - 1) (counter + 1) (result + a + (sequence1 0 power b 0))
        where
            sequence1 :: Int -> Int -> Int -> Int -> Int
            sequence1 _result _power b _end
             | _power == _end - 1 = _result
             | otherwise = sequence1 (_result + (2 ^ _power) * b) (_power - 1) b _end