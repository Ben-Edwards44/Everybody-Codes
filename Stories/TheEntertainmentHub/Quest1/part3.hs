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
getFinalPos grid startSlot = simulate grid (getStartPos grid startSlot)


getCoins :: [String] -> Int -> String -> Int
getCoins grid startPos behaviours = max (posToSlot (getFinalPos grid startPos behaviours) * 2 - posToSlot (0, startPos)) 0


getStartSlots :: [String] -> [Int]
getStartSlots grid = [0,2..(length (head grid))]


excludeInx :: [t] -> Int -> [t]
excludeInx items inx = take inx items ++ drop (inx + 1) items


getMax' :: [String] -> [String] -> [Int] -> Int -> Int
getMax' grid allBehaviours allStarts tryStartInx
    | tryStartInx >= length allStarts = 0
    | null allBehaviours || null allStarts = 0
    | otherwise = max (getCoins grid (allStarts !! tryStartInx) (head allBehaviours) + getMax grid (tail allBehaviours) (excludeInx allStarts tryStartInx)) (getMax' grid allBehaviours allStarts (tryStartInx + 1))


getMax :: [String] -> [String] -> [Int] -> Int
getMax grid allBehaviours allStarts = getMax' grid allBehaviours allStarts 0


getMin' :: [String] -> [String] -> [Int] -> Int -> Int
getMin' grid allBehaviours allStarts tryStartInx
    | tryStartInx == length allStarts - 1 = getCoins grid (allStarts !! tryStartInx) (head allBehaviours) + getMin grid (tail allBehaviours) (excludeInx allStarts tryStartInx)
    | null allBehaviours || null allStarts = 0
    | otherwise = min (getCoins grid (allStarts !! tryStartInx) (head allBehaviours) + getMin grid (tail allBehaviours) (excludeInx allStarts tryStartInx)) (getMin' grid allBehaviours allStarts (tryStartInx + 1))


getMin :: [String] -> [String] -> [Int] -> Int
getMin grid allBehaviours allStarts = getMin' grid allBehaviours allStarts 0


main :: IO()
main = do
    contents <- readFile "part3.txt"
    let allLines = lines contents
    let gap = getInputGap allLines
    let grid = getGrid allLines gap
    let allBehaviours = getBehaviours allLines gap
    let availableStarts = getStartSlots grid

    let maxScore = getMax grid allBehaviours availableStarts
    let minScore = getMin grid allBehaviours availableStarts

    putStrLn (show minScore ++ " " ++ show maxScore)