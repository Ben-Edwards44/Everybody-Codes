numRepeats :: Int
numRepeats = 100000


colourToInt :: Char -> Int
colourToInt colour
    | colour == 'R' = 0
    | colour == 'G' = 1
    | colour == 'B' = 2


repeatList :: [t] -> Int -> [t]
repeatList items n = if n <= 0 then [] else items ++ repeatList items (n - 1)


getMidInx :: [Int] -> Int
getMidInx items = div (length items - 1) 2


getFrontHalf :: [Int] -> [Int]
getFrontHalf items = take (getMidInx items + 1) items


getBackHalf :: [Int] -> [Int]
getBackHalf items = drop (getMidInx items + 1) items


hitsDouble :: Int -> Int -> Bool
hitsDouble arrowColour frontBalloon = arrowColour == frontBalloon


getSecondHalf' :: Int -> [Int] -> [Int] -> [Int]
getSecondHalf' arrowColour [] backHalf = backHalf
getSecondHalf' arrowColour frontHalf [] = []
getSecondHalf' arrowColour (frontHalfHead:frontHalfTail) (backHalfHead:backHalfTail)
    | hitsDouble arrowColour frontHalfHead = getSecondHalf' ((arrowColour + 1) `mod` 3) frontHalfTail backHalfTail
    | otherwise = if not (null frontHalfTail) then backHalfHead : getSecondHalf' ((arrowColour + 2) `mod` 3) (tail frontHalfTail) backHalfTail else backHalfHead:backHalfTail --move along 2 arrows


getSecondHalf :: [Int] -> Int -> [Int]
getSecondHalf balloonColours arrowColour = getSecondHalf' arrowColour (getFrontHalf balloonColours) (getBackHalf balloonColours)  --assumes even length of balloonColours


divideAndConquer :: [Int] -> Int -> Int
divideAndConquer [] arrowColour = 0
divideAndConquer balloonColours arrowColour
    | even (length balloonColours) = getLengthFront balloonColours + divideAndConquer (getSecondHalf balloonColours arrowColour) ((arrowColour + getLengthFront balloonColours) `mod` 3)
    | otherwise = 1 + divideAndConquer (tail balloonColours) ((arrowColour + 1) `mod` 3)
    where
        getLengthFront balloonColours = div (length balloonColours) 2


main :: IO()
main = do
    balloonStr <- readFile "part3.txt"
    let balloonSequence = map colourToInt balloonStr
    let balloonColours = repeatList balloonSequence numRepeats

    let r = divideAndConquer balloonColours 0

    print r
