main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    print $ hasIncDigits 12445 == True
    print $ hasIncDigits 6445 == False 

hasIncDigits :: Int -> Bool
hasIncDigits num
 | num < 10 = True
 | otherwise = helper num 10
    where
        helper :: Int -> Int -> Bool
        helper thisNum result
         | (mod thisNum 10) < 10 && thisNum <= result = True
         | (mod thisNum 10) > result = False
         | otherwise = helper (div thisNum 10) (mod thisNum 10)