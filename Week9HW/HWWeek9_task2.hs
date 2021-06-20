main :: IO()
main = do
    print $ coldestCountry [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 13), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "France"
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ getHeight (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])
    
    -- print $ getAvg (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])
    -- print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] -- == "Bulgaria"
    -- print $ getCapital (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])
    -- print $ getCityName (City "Varna" 0 16)
    -- print $ averageCountryTemp (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])
    -- print $ averageCountryTemp (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)])
    -- print $ averageCountryTemp (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])
    -- print $ (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City a = City Name Elevation AvgYearlyTemperature
 deriving (Show)
data Country a = Country Name Capital [City a]
 deriving (Show)


getCountryName :: Country a -> Name
getCountryName (Country name _ _) = name

getCityName :: City a -> Name
getCityName (City n _ _) = n

getCityTemp :: City a -> AvgYearlyTemperature
getCityTemp (City _ _ temp) = temp


-- This task returns the countru with coldest avt temp
averageCountryTemp :: Country a -> Double
averageCountryTemp (Country name cap (x@(City n el temp):xs)) = (foldr1 (+) [temp | (City _ _ temp) <- (x:xs)]) / (fromIntegral $ length (x:xs))

coldestCountry :: [Country a] -> Name
coldestCountry (x@(Country name cap (y@(City n el temp):ys)):xs) = getCountryName $ foldr1 (\ c1 c2 -> if averageCountryTemp c1 < averageCountryTemp c2 then c1 else c2) (x:xs)
--------------------------------------------------------------


getCapital :: Country a -> City a
getCapital (Country name cap (x@(City n el temp):xs)) = (filter (\ y -> getCityName y == cap) (x:xs)) !! 0

coldestCapital :: [Country a] -> Name
coldestCapital (x@(Country name cap (y@(City n el temp):ys)):xs) = getCountryName $ foldr1 (\ c1 c2 -> if getCityTemp (getCapital c1) < getCityTemp (getCapital c2) then c1 else c2) (x:xs)

-- getAvg :: Country a -> AvgYearlyTemperature
-- getAvg (Country _ _ xs) = sum [ avgTemp | (City _ _ avgTemp) <- xs] 

-- highestCapital :: [Country a] -> Name
-- highestCapital xs = helper (map (\ x -> getCapital x) xs)
--  where
--      helper :: [City a] -> Name
--      helper ys = getCityName $ foldr1 (\ c1 c2 -> if getHeight c1 > getHeight c2 then c1 else c2) ys


-- getHeight  :: City a -> Elevation
-- getHeight (City _ el _) = el


getHeight  :: Country a -> Elevation
getHeight (Country _ capitalName xs)
 | resElevation == [] = error "No data for the capital"
 | otherwise = head resElevation
  where
      resElevation :: [Elevation]
      resElevation = map (\ (City _ elevation _) -> elevation) $ filter (\ (City name _ _) -> name == capitalName) xs