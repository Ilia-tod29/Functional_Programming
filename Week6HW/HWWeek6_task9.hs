import Data.Char
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    --                                                          ^^                 ^^                   ^^
    print $ reduceStr "aAbBcC" == ""


deleteAt :: Int -> String -> String
deleteAt idx xs = left ++ right
  where (left, (_:right)) = splitAt idx xs

reduceStr :: String -> String
reduceStr str = reduceHelper str 0 False
 where
     reduceHelper :: String -> Int -> Bool -> String
     reduceHelper xs index flag
      | length xs - 1 <= index && not flag = xs
      | flag = reduceHelper (deleteAt index xs) 0 False
      | (xs !! index /= xs !! (index + 1)) && 
       (toUpper(xs !! index) == xs !! (index + 1) || 
       xs !! index == toUpper (xs !! (index + 1))) = reduceHelper (deleteAt index xs) index True
      | otherwise = reduceHelper xs (index + 1) False