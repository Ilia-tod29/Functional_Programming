import Data.List
main :: IO()
main = do
    print $ (pairCompose [(\ x -> x + 1), (\ x -> x + 2), (\ x -> x + 3)]) 1

pairCompose :: (Num a) => [(a -> a)] -> (a -> a)
pairCompose fs
 | length fs == 1 = fs !! 0
 | length fs == 2 = (fs !! 0) . (fs !! 1)
 | otherwise =  (\ x -> ((fs !! 0) . (fs !! 1)) x + pairCompose (drop 2 fs) x)