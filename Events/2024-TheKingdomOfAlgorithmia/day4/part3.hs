parseNailHeights :: [String] -> [Int]
parseNailHeights = map read


sort :: Ord t => [t] -> [t]
sort [] = []
sort (x:xs) = sort (filter (<x) xs) ++ [x] ++ sort (filter (>=x) xs)


getMedian :: [Int] -> Int
getMedian list = sort list !! mid
    where
        mid = div (length list) 2


getTotal :: [Int] -> Int -> Int
getTotal [] _ = 0
getTotal (x:xs) smallest = abs (x - smallest) + getTotal xs smallest


main :: IO()
main = do
    input <- readFile "part3.txt"
    let nailHeights = parseNailHeights (lines input)
    let total = getTotal nailHeights (getMedian nailHeights)

    print total