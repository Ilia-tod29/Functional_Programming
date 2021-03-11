main :: IO()
main = do
    print $ maxMultiple 2 7 == 6
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98

maxMultiple :: Int -> Int -> Int
maxMultiple div bound
 | div <= 0 || bound <= 0 = error "Please isert positive numbers"
 | otherwise = helper div
  where
      helper :: Int -> Int
      helper result
       | result > bound = result - div
       | otherwise = helper (result + div)