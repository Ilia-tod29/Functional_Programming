main :: IO()
main = do
    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

countDigitsRec :: Int -> Int
countDigitsRec num 
 | num < 0 = error "Your number was negative"
 | num < 10 = 1
 | otherwise = 1 + countDigitsRec (div num 10)


countDigitsIter :: Int -> Int
countDigitsIter num
 | num < 0 = error "Yout number was negative"
 | otherwise = helper num 1
    where
        helper :: Int -> Int -> Int
        helper thisNum result
         | thisNum < 10 = result
         | otherwise = helper (div thisNum 10) (result + 1)