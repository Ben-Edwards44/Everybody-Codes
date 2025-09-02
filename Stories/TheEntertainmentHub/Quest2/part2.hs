colourToInt :: Char -> Int
colourToInt colour
    | colour == 'R' = 0
    | colour == 'G' = 1
    | colour == 'B' = 2


repeatList :: [t] -> Int -> [t]
repeatList items n = if n <= 0 then [] else items ++ repeatList items (n - 1)


getMidInx :: [Int] -> Int
getMidInx items = div (length items - 1) 2


removeInx :: Int -> [Int] -> [Int]
removeInx inx items = take inx items ++ drop (inx + 1) items


removeMid :: [Int] -> [Int]
removeMid items = removeInx (getMidInx items) items


fireArrow :: Int -> [Int] -> [Int]
fireArrow arrowColour (hitBalloon:otherBalloons)
    | arrowColour /= hitBalloon || even (length otherBalloons) = otherBalloons
    | otherwise = removeMid otherBalloons


getNumArrows :: Int -> [Int] -> Int
getNumArrows arrowColour [] = 0
getNumArrows arrowColour balloonColours = 1 + getNumArrows ((arrowColour + 1) `mod` 3) (fireArrow arrowColour balloonColours)


main :: IO()
main = do
    balloonStr <- readFile "part2.txt"
    let balloonColours = repeatList (map colourToInt balloonStr) 100
    let numArrows = getNumArrows 0 balloonColours

    print numArrows