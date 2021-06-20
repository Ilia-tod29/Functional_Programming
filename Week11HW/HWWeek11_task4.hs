main :: IO()
main = do
    print $ closestToAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 6 -- could be 11 or 21
    print $ findAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]
    
data Measuring = Temp Int Float
 deriving(Show)

findAverage :: [Measuring] -> Float
findAverage temps = (foldr1 (+) (map (\ (Temp x y) -> y) temps)) / (fromIntegral $ length temps)

getClosestMeasureDay :: Measuring -> Int
getClosestMeasureDay (Temp day temp) = day 

closestToAverage :: [Measuring] -> Int
closestToAverage temps = getClosestMeasureDay (temps !! snd(foldr1 (\ t1 t2 -> if abs (fst t1) <= abs (fst t2) then t1 else t2) (zip (map (\ new -> (findAverage temps) - new) [y | (Temp x y) <- temps]) [0, 1 ..])))