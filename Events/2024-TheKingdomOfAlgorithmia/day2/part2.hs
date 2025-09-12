split' :: String -> Char -> String -> [String]
split' [] _ current = [current]
split' (x:xs) seperator current
    | x == seperator = current : split' xs seperator ""
    | otherwise = split' xs seperator (current ++ [x])


split :: Char -> String -> [String]
split seperator string = split' string seperator ""


getRuneWords :: [String] -> [String]
getRuneWords inputLines = split ',' (drop 6 (head inputLines))


getInscriptionWords :: [String] -> [[String]]
getInscriptionWords inputLines = map (split ' ') (tail (tail inputLines))


initList :: t -> Int -> [t]
initList item len
    | len <= 0 = []
    | otherwise = item : initList item (len - 1)


count :: Eq t => t -> [t] -> Int
count _ [] = 0
count item (x:xs) = if x == item then 1 + count item xs else count item xs


overlap :: [Bool] -> [Bool] -> [Bool]
overlap smaller larger = zipWith (||) smaller larger ++ drop (length smaller) larger


slidingWindow :: String -> Int -> String -> [Bool]
slidingWindow runeWord start parentWord
    | start >= length parentWord = []
    | isEqual = True : overlap (initList True (length runeWord - 1)) (slidingWindow runeWord (start + 1) parentWord)
    | otherwise = False : slidingWindow runeWord (start + 1) parentWord
    where
        current = take (length runeWord) (drop start parentWord)
        isEqual = current == runeWord || reverse current == runeWord


findNumRune :: [String] -> String -> [Bool]
findNumRune parentWords runeWord = foldr (++) [] (map (slidingWindow runeWord 0) parentWords)


overlapRunes :: [[Bool]] -> [Bool]
overlapRunes = foldl overlap []


findTotalNumRune :: [String] -> [String] -> Int
findTotalNumRune runeWords parentWords = count True (overlapRunes (map (findNumRune parentWords) runeWords))


findRunesAcrossLines :: [[String]] -> [String] -> Int
findRunesAcrossLines allParentLines runeWords = sum (map (findTotalNumRune runeWords) allParentLines)


main :: IO()
main = do
    input <- readFile "part2.txt"
    let inputLines = lines input
    let runeWords = getRuneWords inputLines
    let inscriptionWords = getInscriptionWords inputLines
    let output = findRunesAcrossLines inscriptionWords runeWords

    print output