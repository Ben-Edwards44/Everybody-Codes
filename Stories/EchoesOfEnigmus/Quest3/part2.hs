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


checkOnGoldenLine :: [(Int, Int)] -> Bool
checkOnGoldenLine = all (\x -> snd x == 1)


stepUntilGolden :: [(Int, Int)] -> Int -> Int
stepUntilGolden coords numSteps
    | checkOnGoldenLine (stepCoords numSteps coords) = numSteps
    | otherwise = stepUntilGolden coords (numSteps + 1)


main :: IO()
main = do
    input <- readFile "part2.txt"
    let inputLines = lines input
    let coords = getAllCoords inputLines
    let numDays = stepUntilGolden coords 1

    print numDays
