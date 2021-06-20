main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\(_,_,city) -> city == "Burgas") == 24.95
    print $ getAverageBalance (accounts1, people1) (\(_,(n:_),_) -> n == 'P') == 18.85
    print $ getAverageBalance (accounts1, people1) (\ (n,_,_) -> n ==2) == 26.8
    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62
    print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25

type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1, "Ivan", "Sofia"),(2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"),(4, "Petya", "Burgas")]

accounts1 :: [Account]
accounts1 = [(1, 1, 12.5),(2, 1, 123.2),(3, 2, 13.0),(4, 2, 50.2),(5, 2, 17.2),(6, 3, 18.3),(7, 4, 19.4)]



round2 :: Double -> Double
round2 x = (fromIntegral (round (x * 10^2))) / 10^2




filterAcc :: [Account] -> [Person] -> [Account] -> [Account] 
filterAcc as ((id, name, city):ys) resList
 | ys == [] = resList ++ filter (\ (_,x,_) -> x == id) as
 | otherwise = filterAcc as ys (resList ++ filter (\ (_,x,_) -> x == id) as)
 
totalAvg ((x, y, amount):xs) sum counter
 | xs == [] = round2 $ (sum + amount) / (counter + 1)
 | otherwise = totalAvg xs (sum + amount) (counter + 1)

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (_, []) _ = 0
getAverageBalance ([], _) _ = 0
getAverageBalance (as, ps) pred = totalAvg (filterAcc as (filter (\ person -> pred person) ps) []) 0 0

averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities (as, ps) cities = getAverageBalance (as, ps) (\ (_,_,city) -> (any (\ x -> city == x) cities))