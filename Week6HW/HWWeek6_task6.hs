main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2


getOddCompositionValue :: [(a -> a)] -> (a -> a)
getOddCompositionValue xs = foldr1 (.) [x | (x, index) <- zip xs [1 ..], odd index]