import Data.Char
main :: IO()
main = do
    print $ nthSpecialPrime 5 2 
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> mod n x /= 0) [2 .. n - 1]

nthSpecialPrime :: Int -> Int -> Int
nthSpecialPrime n d = helper 2 0
 where
     helper :: Int -> Int -> Int
     helper currentPrime counter
      | counter == n = currentPrime - 1
      | isPrime currentPrime && elem d [digitToInt x | x <- (show currentPrime)] = helper (currentPrime + 1) (counter + 1)
      | otherwise = helper (currentPrime + 1) (counter)

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ filter (\ x -> isPrime x && elem d [digitToInt q | q <- (show x)]) [0 .. nthSpecialPrime n d]