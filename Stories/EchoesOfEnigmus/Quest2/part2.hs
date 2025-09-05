split' :: String -> Char -> String -> [String]
split' [] _ current = [current]
split' (x:xs) seperator current
    | x == seperator = current : split' xs seperator ""
    | otherwise = split' xs seperator (current ++ [x])


split :: String -> Char -> [String]
split string seperator = split' string seperator ""


loseEnds :: [t] -> [t]
loseEnds items = drop 1 (take (length items - 1) items)


buildNode :: String -> NodeData Char
buildNode string = NodeData parseRank parseData
    where
        parseRank = read (head (split string ','))
        parseData = head (split string ',' !! 1)


parseNode :: Int -> Int -> String -> NodeData Char
parseNode inx1 prefixLength string = buildNode (loseEnds getNodeString)
    where
        getNodeString = drop prefixLength (split string ' ' !! inx1)


getLeftNode :: String -> NodeData Char
getLeftNode = parseNode 2 5


getRightNode :: String -> NodeData Char
getRightNode = parseNode 3 6


getId :: String -> Int
getId inputLine
    | isAddOperation inputLine = read (drop 3 (split inputLine ' ' !! 1))
    | otherwise = read (split inputLine ' ' !! 1)


data NodeData t = Eq t => NodeData Int t
data BinaryTree t = Eq t => WithoutNodes | WithNodes (BinaryTree t) (BinaryTree t) (NodeData t)


getRank :: NodeData t -> Int
getRank (NodeData rank _) = rank


getData :: NodeData t -> t
getData (NodeData _ d) = d


nodesEqual :: Eq t => NodeData t -> NodeData t -> Bool
nodesEqual node1 node2 = getRank node1 == getRank node2 && getData node1 == getData node2


printNode :: (Show t) => NodeData t -> String
printNode node = "[" ++ show (getRank node) ++ "," ++ show (getData node) ++ "]"


printTree :: (Show t) => BinaryTree t -> String
printTree WithoutNodes = ""
printTree (WithNodes left right root) = printNode root ++ "\nL: " ++ printTree left ++ "\nR: " ++ printTree right


addNode :: BinaryTree t -> NodeData t -> BinaryTree t
addNode WithoutNodes node = WithNodes WithoutNodes WithoutNodes node          --base case
addNode (WithNodes left right root) node
    | getRank node < getRank root = WithNodes (addNode left node) right root  --add to left
    | otherwise = WithNodes left (addNode right node) root                    --add to right


getNodesAtLevel :: BinaryTree t -> Int -> [NodeData t]
getNodesAtLevel WithoutNodes _ = []
getNodesAtLevel (WithNodes left right root) level
    | level < 0 = []
    | level == 0 = [root]
    | otherwise = getNodesAtLevel left (level - 1) ++ getNodesAtLevel right (level - 1)


longestList :: [t] -> [t] -> [t]
longestList a b = if length a > length b then a else b


getMaxNodes' :: BinaryTree t -> Int -> [NodeData t]
getMaxNodes' tree level
    | null (getNodesAtLevel tree level) = []  --end of tree reached
    | otherwise = longestList (getNodesAtLevel tree level) (getMaxNodes' tree (level + 1))


getMaxNodes :: BinaryTree t -> [NodeData t]
getMaxNodes tree = getMaxNodes' tree 0


getLongestString :: BinaryTree Char -> String
getLongestString tree = map getData (getMaxNodes tree)


isAddOperation :: String -> Bool
isAddOperation inputLine = take 3 inputLine == "ADD"


getAllNodes :: (String -> NodeData t) -> [String] -> [NodeData t]
getAllNodes nodeFunc inputLines = map nodeFunc (filter isAddOperation inputLines)


swap :: Eq t => Show t => BinaryTree t -> NodeData t -> NodeData t -> BinaryTree t
swap WithoutNodes _ _ = WithoutNodes
swap (WithNodes left right root) node1 node2
    | nodesEqual root node1 = WithNodes left right node2
    | nodesEqual root node2 = WithNodes left right node1
    | otherwise = WithNodes (swap left node1 node2) (swap right node1 node2) root


runCommand :: Eq t => Show t => BinaryTree t -> [NodeData t] -> [NodeData t] -> String -> BinaryTree t
runCommand tree currentNodes otherNodes command
    | isAddOperation command = addNode tree (currentNodes !! nodeInx)
    | otherwise = swap tree (currentNodes !! nodeInx) (otherNodes !! nodeInx)
    where
        nodeInx = getId command - 1


runAllCommands :: Eq t => Show t => BinaryTree t -> [NodeData t] -> [NodeData t] -> [String] -> BinaryTree t
runAllCommands tree _ _ [] = tree
runAllCommands tree currentNodes otherNodes (commandHead:otherCommands) = runAllCommands newTree currentNodes otherNodes otherCommands
    where
        newTree = runCommand tree currentNodes otherNodes commandHead


main :: IO()
main = do
    input <- readFile "part2.txt"
    let inputLines = lines input
    let leftNodes = getAllNodes getLeftNode inputLines
    let rightNodes = getAllNodes getRightNode inputLines

    let firstTree = runAllCommands WithoutNodes leftNodes rightNodes inputLines
    let secondTree = runAllCommands WithoutNodes rightNodes leftNodes inputLines

    putStrLn (getLongestString firstTree ++ getLongestString secondTree)