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


getNeed :: (Int, Int) -> Int -> Int -> Int -> Int
getNeed coord length smallStep n
    | mod current smallStep == 0 = current
    | otherwise = getNeed coord length smallStep (n + 1)
    where
        current = snd coord - 1 + length * n


getStepsUntilEnd :: (Int, Int) -> Int -> Int
getStepsUntilEnd coord stepSize = div (getNeed coord length smallStep 0) smallStep
    where
        length = getLengthDiag coord
        smallStep = mod stepSize length


advanceDays :: [(Int, Int)] -> Int -> Int -> Int
advanceDays [] dayNum _ = dayNum
advanceDays (currentCoord:otherCoords) dayNum stepSize
    | checkOnGoldenLine (currentCoord:otherCoords) = dayNum
    | otherwise = advanceDays (stepCoords (newDayNum - dayNum) otherCoords) newDayNum newStepSize
    where
        newDayNum = dayNum + stepSize * getStepsUntilEnd currentCoord stepSize
        newStepSize = lcm stepSize (getLengthDiag currentCoord)


sortCoords :: [(Int, Int)] -> [(Int, Int)]
sortCoords [] = []
sortCoords (pivot:other) = sortCoords upper ++ [pivot] ++ sortCoords lower
    where
        lower = filter (\x -> getLengthDiag x <= getLengthDiag pivot) other
        upper = filter (\x -> getLengthDiag x > getLengthDiag pivot) other


main :: IO()
main = do
    input <- readFile "part3.txt"
    let inputLines = lines input
    let coords = sortCoords (getAllCoords inputLines)

    let totalDays = advanceDays coords 0 1

    print totalDays