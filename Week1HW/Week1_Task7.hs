main :: IO()
main = do
    print $ growingPlant 5 2 5 0 == 1
    print $ growingPlant 5 2 6 0 == 2
    print $ growingPlant 10 9 4 0 == 1
    print $ growingPlant 100 10 910 0 == 10

--Don't know how to add initial valie of a parmeter, so I added it in the call :(
growingPlant :: Int -> Int -> Int -> Int -> Int 
growingPlant upSpeed downSpeed desiredSpeed meters
 | meters == desiredSpeed || meters > desiredSpeed = 0
 | otherwise = 1 + growingPlant upSpeed downSpeed desiredSpeed (meters + upSpeed)