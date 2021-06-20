main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City a = City Name Elevation AvgYearlyTemperature
 deriving (Show)
data Country a = Country Name Capital [City a]
 deriving (Show)

getCountryName :: Country a -> Name
getCountryName (Country name cap (y@(City n el temp):ys)) = name

getCityName :: City a -> Name
getCityName (City n el temp) = n

getCityElevation :: City a -> Elevation
getCityElevation (City n el temp) = el

getCapital :: Country a -> City a
getCapital (Country name cap (x@(City n el temp):xs)) = (filter (\ y -> getCityName y == cap) (x:xs)) !! 0

highestCapital :: [Country a] -> Name
highestCapital (x@(Country name cap (y@(City n el temp):ys)):xs) = getCountryName $ foldr1 (\ c1 c2 -> if getCityElevation (getCapital c1) > getCityElevation (getCapital c2) then c1 else c2) (x:xs)