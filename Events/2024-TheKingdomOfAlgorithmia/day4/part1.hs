parseNailHeights :: [String] -> [Int]
parseNailHeights = map read


getMinItem :: [Int] -> Int
getMinItem list = foldr min (last list) list


getTotal :: [Int] -> Int -> Int
getTotal [] _ = 0
getTotal (x:xs) smallest = (x - smallest) + getTotal xs smallest


main :: IO()
main = do
    input <- readFile "part1.txt"
    let nailHeights = parseNailHeights (lines input)
    let total = getTotal nailHeights (getMinItem nailHeights)

    print total