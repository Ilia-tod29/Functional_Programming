main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]
    print $ round2 pi

type Cylinder = (Double, Double)


round2 :: Double -> Double
round2 x = (fromIntegral (round (x * 10^2))) / 10^2

getVolumes :: [Cylinder] -> [Double]
getVolumes xs = map (\ (r, h) -> round2 (pi * r * r * h) ) xs