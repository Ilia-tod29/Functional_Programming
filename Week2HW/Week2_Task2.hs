main :: IO()
main = do
    -- print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 62345 == 20
    print $ sumDigitsIter 965 == 20

sumDigitsIter :: Int -> Int
sumDigitsIter num
 | num < 0 = error "Your number was negative"
 | otherwise = helper num 0
    where
        helper :: Int -> Int -> Int 
        helper thisNum result
         | thisNum < 10 = result + thisNum
         | otherwise = helper (div thisNum 10) (result + mod thisNum 10)