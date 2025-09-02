colourToInt :: Char -> Int
colourToInt colour
    | colour == 'R' = 0
    | colour == 'G' = 1
    | colour == 'B' = 2


getNumArrows :: Int -> [Int] -> Int
getNumArrows arrowColour [] = 1
getNumArrows arrowColour (hitBalloon:otherBalloons)
    | null otherBalloons = 1
    | arrowColour == hitBalloon = getNumArrows arrowColour otherBalloons
    | otherwise = 1 + getNumArrows ((arrowColour + 1) `mod` 3) otherBalloons


main :: IO()
main = do
    balloonStr <- readFile "part1.txt"
    let balloonColours = map colourToInt balloonStr
    let numArrows = getNumArrows 0 balloonColours

    print numArrows