main :: IO()
main = do
    print $ isSquare 1 == True
    print $ isSquare 2 == False
    print $ isSquare 4 == True
    print $ isSquare 17 == False
    print $ isSquare 256 == True
    print $ isSquare 2500 == True

isSquare :: Int -> Bool
isSquare 1 = True
isSquare x
 | x < 0 = error "x must be positive"
 | otherwise = helper 0
  where
      helper :: Int -> Bool
      helper check
       | check == div x 2 && check * check == x = True
       | check == div x 2 = False
       | check * check == x = True
       | otherwise = helper (check + 1)