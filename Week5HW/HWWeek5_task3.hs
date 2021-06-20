import Data.List
import Data.Char
main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

specialSum :: Int -> Int -> Int
specialSum x y = sum $ filter (\ a -> any (\ b -> b == '6') (show a) && mod (a - 1) 4 == 0) [min x y .. max x y]