main :: IO()
main = do
    print $ switchsum (\ x-> x + 1) (\ x -> x * 2) 1 $ 2 
    print $ switchsum (\ x-> x + 1) (\ x -> x * 2) 2 $ 2 
    print $ switchsum (\ x-> x + 1) (\ x -> x * 2) 3 $ 2 
    print $ switchsum (\ x-> x + 1) (\ x -> x * 2) 4 $ 2 


switchsum :: (Num a) => (a -> a) -> (a -> a) -> Int -> (a -> a)
switchsum f g n
 | n == 1 = f
 | otherwise = (\ x -> f x + switchsum g f (n - 1) (f x))