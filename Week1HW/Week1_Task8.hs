main :: IO()
main = do
    print $ snail 3 2 1 0 == 2
    print $ snail 10 3 1 0 == 5
    print $ snail 10 3 2 0 == 8
    print $ snail 100 20 5 0 == 7
    print $ snail 5 10 3 0 == 1
--Don't know how to add initial valie of a parmeter, so I added it in the call :(
snail :: Int -> Int -> Int -> Int -> Int 
snail column up down current
 | current + up >= column = 1
 | otherwise = 1 + snail column up down (current + up - down)