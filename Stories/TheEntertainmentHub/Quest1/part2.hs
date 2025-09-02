isNail :: [String] -> (Int, Int) -> Bool
isNail grid pos = (grid !! fst pos) !! snd pos == '*'


isAtBottom :: [String] -> (Int, Int) -> Bool
isAtBottom grid pos = fst pos >= length grid


goDown :: (Int, Int) -> (Int, Int)
goDown pos = (fst pos + 1, snd pos)


getNailPos :: [String] -> (Int, Int) -> (Int, Int)
getNailPos grid pos =   if isAtBottom grid pos || isNail grid pos
                            then pos 
                        else getNailPos grid (goDown pos)


left :: (Int, Int) -> (Int, Int)
left pos = (fst (goDown pos), snd pos - 1)


right :: (Int, Int) -> (Int, Int)
right pos = (fst (goDown pos), snd pos + 1)


goLeft :: [String] -> (Int, Int) -> Char -> Bool
goLeft grid pos behaviour
    | snd pos == 0 = False
    | snd pos == length (head grid) - 1 = True
    | otherwise = behaviour == 'L'


nextPos :: [String] -> (Int, Int) -> Char -> (Int, Int)
nextPos grid pos behaviour = if goLeft grid pos behaviour
    then getNailPos grid (left pos)
    else getNailPos grid (right pos)


simulate :: [String] -> (Int, Int) -> String -> (Int, Int)
simulate grid pos behaviours =  if isAtBottom grid pos
                                    then pos
                                else simulate grid (nextPos grid pos (head behaviours)) (tail behaviours)


getStartPos :: [String] -> Int -> (Int, Int)
getStartPos grid startSlot = getNailPos grid (0, startSlot)


posToSlot :: (Int, Int) -> Int
posToSlot pos = div (snd pos) 2 + 1


getFinalPos :: [String] -> Int -> String -> (Int, Int)
getFinalPos grid startSlot behaviours = simulate grid (getStartPos grid startSlot) behaviours


getCoins :: [String] -> Int -> String -> Int
getCoins grid startPos behaviours = max (posToSlot (getFinalPos grid startPos behaviours) * 2 - posToSlot (0, startPos)) 0


getMaxNumCoins' :: [String] -> String -> Int -> Int
getMaxNumCoins' grid behaviour tryPos = if tryPos < length (head grid)
                                            then max (getCoins grid tryPos behaviour) (getMaxNumCoins' grid behaviour (tryPos + 2))
                                        else 0


getMaxNumCoins :: [String] -> String -> Int
getMaxNumCoins grid behaviour = getMaxNumCoins' grid behaviour 0


getStartSlots :: [String] -> [Int]
getStartSlots grid = [0,2..(length (head grid))]


getSum :: [String] -> [String] -> Int
getSum grid [] = 0
getSum grid (allBehavioursH:allBehavioursT) = (getMaxNumCoins grid allBehavioursH) + getSum grid allBehavioursT


getInputGap' :: [String] -> Int -> Int
getInputGap' fileLines inx =    if null (fileLines !! inx)
                                    then inx
                                else getInputGap' fileLines (inx + 1)

getInputGap :: [String] -> Int
getInputGap fileLines = getInputGap' fileLines 0


getGrid :: [String] -> Int -> [String]
getGrid fileLines gap = take gap fileLines


getBehaviours :: [String] -> Int -> [String]
getBehaviours fileLines gap = drop (gap + 1) fileLines


main :: IO()
main = do
    contents <- readFile "part2.txt"
    let allLines = lines contents
    let gap = getInputGap allLines
    let grid = getGrid allLines gap
    let allBehaviours = getBehaviours allLines gap
   
    let sum = getSum grid allBehaviours
    
    print sum