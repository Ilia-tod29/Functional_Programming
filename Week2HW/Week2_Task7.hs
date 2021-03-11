main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 411 == False
    print $ isInteresting 468 == True
    print $ isInteresting 415 == False
    print $ isInteresting 550 == True


sumDigits :: Int -> Int
sumDigits num
 | num < 0 = error "Your number was negative"
 | otherwise = helper num 0
    where
        helper :: Int -> Int -> Int 
        helper thisNum result
         | thisNum < 10 = result + thisNum
         | otherwise = helper (div thisNum 10) (result + mod thisNum 10)

isInteresting :: Int -> Bool
isInteresting num = if mod num (sumDigits (num)) == 0 then True else False