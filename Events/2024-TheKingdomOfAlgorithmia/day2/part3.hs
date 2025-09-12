split' :: String -> Char -> String -> [String]
split' [] _ current = [current]
split' (x:xs) seperator current
    | x == seperator = current : split' xs seperator ""
    | otherwise = split' xs seperator (current ++ [x])


split :: Char -> String -> [String]
split seperator string = split' string seperator ""


getRuneWords :: [String] -> [String]
getRuneWords inputLines = split ',' (drop 6 (head inputLines))


getInscriptionWords :: [String] -> [String]
getInscriptionWords inputLines = tail (tail inputLines)


initList :: t -> Int -> [t]
initList item len
    | len <= 0 = []
    | otherwise = item : initList item (len - 1)


count :: Eq t => t -> [t] -> Int
count _ [] = 0
count item (x:xs) = if x == item then 1 + count item xs else count item xs


overlap :: [Bool] -> [Bool] -> [Bool]
overlap smaller larger = zipWith (||) smaller larger ++ drop (length smaller) larger


overlapMasks :: [[Bool]] -> [Bool]
overlapMasks = foldl overlap []


extractSubstring :: String -> Int -> Int -> String
extractSubstring parentString start len = if len <= 0 then [] else (parentString !! start) : extractSubstring parentString newStart (len - 1)
    where
        newStart = mod (start + 1) (length parentString)


createMask :: Int -> Int -> Int -> Int -> [Bool]
createMask start runeLength parentLength current
    | current >= parentLength = []
    | distStart < runeLength = True : createMask start runeLength parentLength (current + 1)
    | otherwise = False : createMask start runeLength parentLength (current + 1)
    where
        distStart = if current >= start then current - start else current + parentLength - start


slidingWindow :: Bool -> Int -> String -> String -> [Bool]
slidingWindow wrap start parentWord runeWord
    | start >= length parentWord = initList False (length parentWord)
    | isEqual = overlap (createMask start (length runeWord) (length parentWord) 0) (slidingWindow wrap (start + 1) parentWord runeWord)
    | otherwise = slidingWindow wrap (start + 1) parentWord runeWord
    where
        current = if wrap then extractSubstring parentWord start (length runeWord) else take (length runeWord) (drop start parentWord)
        isEqual = current == runeWord || reverse current == runeWord


rowMask :: String -> String -> [Bool]
rowMask = slidingWindow True 0


colMask :: String -> String -> [Bool]
colMask = slidingWindow False 0


getMask :: (String -> String -> [Bool]) -> [String] -> String -> [Bool]
getMask maskFunc runeWords parentWord = overlapMasks (map (maskFunc parentWord) runeWords)


getRowMasks :: [String] -> [String] -> [[Bool]]
getRowMasks parentWords runeWords = map (getMask rowMask runeWords) parentWords


getCol' :: [[t]] -> Int -> Int -> [t]
getCol' grid colInx rowInx
    | rowInx >= length grid = []
    | otherwise = (grid !! rowInx) !! colInx : getCol' grid colInx (rowInx + 1)


getCol :: [[t]] -> Int -> [t]
getCol grid colInx = getCol' grid colInx 0


getColMasks :: [String] -> [String] -> [[Bool]]
getColMasks parentWords runeWords = map (\x -> getMask colMask runeWords (getCol parentWords x)) [0..(length (head parentWords) - 1)]


overlapRowColMasks' :: [[Bool]] -> [[Bool]] -> Int -> [Bool]
overlapRowColMasks' rowMasks colMasks inx = if inx >= maxInx then [] else current : overlapRowColMasks' rowMasks colMasks (inx + 1)
    where
        width = length (head rowMasks)
        height = length rowMasks
        maxInx = width * height
        rowInx = div inx width
        colInx = mod inx width
        current = (rowMasks !! rowInx) !! colInx || (colMasks !! colInx) !! rowInx


overlapRowColMasks :: [[Bool]] -> [[Bool]] -> [Bool]
overlapRowColMasks rowMasks colMasks = overlapRowColMasks' rowMasks colMasks 0


main :: IO()
main = do
    input <- readFile "part3.txt"
    let inputLines = lines input
    let runeWords = getRuneWords inputLines
    let inscriptionWords = getInscriptionWords inputLines
    let row = getRowMasks inscriptionWords runeWords
    let col = getColMasks inscriptionWords runeWords

    let output = count True (overlapRowColMasks row col)

    print output