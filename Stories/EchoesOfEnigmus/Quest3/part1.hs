split' :: String -> Char -> String -> [String]
split' [] _ current = [current]
split' (strHead:strTail) seperator current
    | strHead == seperator = current : split' strTail seperator ""
    | otherwise = split' strTail seperator (current ++ [strHead])


split :: String -> Char -> [String]
split string seperator = split' string seperator ""


parseCoord :: String -> (Int, Int)
parseCoord coordStr = (getNumPart (head splitted), getNumPart (splitted !! 1))
    where
        splitted = split coordStr ' '
        getNumPart string = read (drop 2 string)


getAllCoords :: [String] -> [(Int, Int)]
getAllCoords = map parseCoord


getLengthDiag :: (Int, Int) -> Int
getLengthDiag pos = fst pos + snd pos - 1


stepAlongDiag :: Int -> (Int, Int) -> (Int, Int)
stepAlongDiag numSteps pos = (newX, newY)
    where
        length = getLengthDiag pos
        newX = mod (fst pos - 1 + numSteps) length + 1
        newY = mod (snd pos - 1 - numSteps) length + 1


stepCoords :: Int -> [(Int, Int)] -> [(Int, Int)]
stepCoords numSteps = map (stepAlongDiag numSteps)


sumFormula :: (Int, Int) -> Int
sumFormula pos = fst pos + 100 * snd pos


getSum :: [(Int, Int)] -> Int
getSum coords = sum (map sumFormula coords)


main :: IO()
main = do
    input <- readFile "part1.txt"
    let inputLines = lines input
    let coords = getAllCoords inputLines
    let stepped = stepCoords 100 coords
    let sum = getSum stepped

    print sum
