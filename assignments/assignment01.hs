import Text.Printf (printf)

hello :: String -> String
hello xs = "Hello " ++ xs

volume :: Double -> Double
volume r = (4/3) * pi * r**3

convertCubicCmToLiters :: Double -> Double
convertCubicCmToLiters x = x * 0.001

roundTo :: Int -> Double -> Double
roundTo n x = read $ printf ("%." ++ show 2 ++ "f") x

formatOutput :: Double -> String
formatOutput x = show(roundTo 2 x) ++ " EUR"

-- heliumCosts :: Double -> String
-- heliumCosts r = formatOutput((convertCubicCmToLiters(volume r) / 400) * 50.99)

heliumCosts :: Double -> Double
heliumCosts r = (convertCubicCmToLiters(volume r) / 400) * 50.99

heliumLitersWorth :: Double -> Double
heliumLitersWorth m = (m / 50.99) * 400

litersToCubicCm :: Double -> Double
litersToCubicCm x = x * 1000

volumeToRadius :: Double -> Double
volumeToRadius v = ((3 * v ) / (4 * pi)) ** (1/3)

formatOutputVolume :: Double -> String
formatOutputVolume x = show(roundTo 2 x) ++ " cm"

-- ballonRadius :: Double -> String 
-- ballonRadius x = formatOutputVolume(volumeToRadius(litersToCubicCm(heliumLitersWorth x)))

ballonRadius :: Double -> Double 
ballonRadius x = volumeToRadius(litersToCubicCm(heliumLitersWorth x))