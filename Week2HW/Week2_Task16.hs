main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6
    print $ reverseOrdSuff 4321

reverseOrdSuff :: Int -> Int
reverseOrdSuff num = helper (div num 10) (mod num 10)
 where
     helper :: Int -> Int -> Int
     helper thisNum result
      | mod thisNum 10 > mod result 10 = helper (div thisNum 10) ((result * 10) + mod thisNum 10)
      | otherwise = result
