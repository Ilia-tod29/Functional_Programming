main :: IO()
main = do
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNum 1253 5123783 == False
    print $ subNum 12 0 == False

xCheck :: Int -> Int -> Bool
xCheck x y
 | x == 0 = True
 | mod x 10 /= mod y 10 = False
 | otherwise = xCheck (div x 10) (div y 10)
 
subNum :: Int -> Int -> Bool
subNum x y
 | x < 0 || y < 0 = error "Pleaseenter only positive numbers"
 | x == y = True
 | y == 0 = False
 | xCheck x y = True
 | otherwise = subNum x (div y 10)