main :: IO()
main = do
    print $ digital_root 16
    print $ digital_root 942
    print $ digital_root 132189
    print $ digital_root 493193
    print $ digital_root 385629103

sumDigits :: Int -> Int
sumDigits x
 | x < 10 = x
 | otherwise = helper x 0
  where
      helper :: Int -> Int -> Int
      helper thisNum result
       | thisNum < 10 = result + (mod thisNum 10)
       | otherwise = helper (div thisNum 10) (result + (mod thisNum 10))


digital_root :: Int -> Int
digital_root x
 | x < 10 = x
 | otherwise = helper (sumDigits x)
  where
      helper :: Int -> Int
      helper result
       | sumDigits result < 10 = sumDigits result
       | otherwise = helper (sumDigits result)