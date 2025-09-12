split' :: String -> Char -> String -> [String]
split' [] _ current = [current]
split' (x:xs) seperator current
    | x == seperator = current : split' xs seperator ""
    | otherwise = split' xs seperator (current ++ [x])


split :: String -> Char -> [String]
split string seperator = split' string seperator ""


getRuneWords :: [String] -> [String]
getRuneWords inputLines = split (drop 6 (head inputLines)) ','


getInscriptionWords :: [String] -> [String]
getInscriptionWords inputLines = split (inputLines !! 2) ' '


slidingWindow :: String -> Int -> Int -> String -> Int
slidingWindow runeWord start end parentWord
    | start >= length parentWord = 0
    | len == length runeWord && isPartEqual = 1 + slidingWindow runeWord (end + 1) (end + 1) parentWord
    | isPartEqual = slidingWindow runeWord start (end + 1) parentWord
    | otherwise = slidingWindow runeWord (end + 1) (end + 1) parentWord
    where
        len = end - start + 1
        isPartEqual = take len (drop start parentWord) == take len runeWord


findNumRune :: [String] -> String -> Int
findNumRune parentWords runeWord = sum (map (slidingWindow runeWord 0 0) parentWords)


findTotalNumRune :: [String] -> [String] -> Int
findTotalNumRune parentWords runeWords = sum (map (findNumRune parentWords) runeWords)


main :: IO()
main = do
    input <- readFile "part1.txt"
    let inputLines = lines input
    let runeWords = getRuneWords inputLines
    let inscriptionWords = getInscriptionWords inputLines
    let output = findTotalNumRune inscriptionWords runeWords

    print output