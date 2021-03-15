main :: IO()
main = do
    print $ removeFistOccurrence 89100 9
    print $ removeFistOccurrence 8910 9
    print $ removeFistOccurrence 8994 9
    print $ removeFistOccurrence 8912 9

-- The problem was in the cases in which the last digit(digits) is(are) zeros. When the last digit is zero after the first call of the function(helper) 'leftDigits' stays the same. 
-- This way we lose one or eventually more zeros.

-- PROBLEM SOLUTION:

-- Description of the solution : Create a function which returns the count of the zeros in the end of the number (e.g. 12300 -> 2; 3214 -> 0).
-- Then add one more case in the 'helper' function which checks if we happened to find the digit we want to remove AND if the given number starts with zero.
-- In this case return the same result, but multipied by 10^{count of zeros}.
combine :: Int -> Int -> Int
combine x 0 = x
combine x y
 | y < 10 = x * 10 + y
 | otherwise = combine (x * 10 + mod y 10) (div y 10)

zerosCount :: Int -> Int 
zerosCount x
 | mod x 10 /= 0 = 0
 | otherwise  = 1 + zerosCount (div x 10)

removeFistOccurrence :: Int -> Int -> Int
removeFistOccurrence n d = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 leftDigits = n
     helper leftOver leftDigits
      | mod leftOver 10 == d && mod n 10 == 0 = (combine (div leftOver 10) leftDigits) * (10 ^ zerosCount n)
      | mod leftOver 10 == d = combine (div leftOver 10) leftDigits
      | otherwise = helper (div leftOver 10) (leftDigits * 10 + mod leftOver 10)