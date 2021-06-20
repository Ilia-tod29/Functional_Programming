main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1


-- google search. Couldn't figure out another way
applyN :: (Num a) => (a -> a) -> Int -> (a -> a)
applyN f x = (\ y -> iterate f y !! x)