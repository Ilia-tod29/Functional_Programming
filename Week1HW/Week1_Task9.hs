main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Int -> Int
rev num = helper num 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = div result 10
     helper num result
      | num < 0 = error "Your number was negative"
      | otherwise = helper (div num 10) ((result + mod num 10) * 10)