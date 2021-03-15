main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51
    print $ p 7 == 70



p :: Int -> Int
p 0 = 0
p 1 = 1
p 2 = 5 
p x 
 | x < 0 = error "The number must be positive!"
 | otherwise = helper 1 4 0
  where
      helper :: Int -> Int -> Int -> Int
      helper result resultAdd counter
       | counter == x - 2 = result + resultAdd
       | otherwise = helper (result + resultAdd) (resultAdd + 3) (counter + 1)