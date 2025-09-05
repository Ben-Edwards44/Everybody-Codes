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
        parseRank = read (head (split string ',')) :: Int
        parseData = head (split string ',' !! 1)


parseNode :: Int -> Int -> String -> NodeData Char
parseNode inx1 prefixLength string = buildNode (loseEnds getNodeString)
    where
        getNodeString = drop prefixLength (split string ' ' !! inx1)


getLeftNode :: String -> NodeData Char
getLeftNode = parseNode 2 5


getRightNode :: String -> NodeData Char
getRightNode = parseNode 3 6


data NodeData t = NodeData Int t
data BinaryTree t = WithoutNodes | WithNodes (BinaryTree t) (BinaryTree t) (NodeData t)


getRank :: NodeData t -> Int
getRank (NodeData rank _) = rank


getData :: NodeData t -> t
getData (NodeData _ d) = d


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


addAllNodes :: [NodeData t] -> BinaryTree t -> BinaryTree t
addAllNodes [] tree = tree
addAllNodes (x:xs) tree = addAllNodes xs (addNode tree x)


main :: IO()
main = do
    input <- readFile "part1.txt"
    let inputLines = lines input
    let leftNodes = map getLeftNode inputLines
    let rightNodes = map getRightNode inputLines

    let firstTree = addAllNodes leftNodes WithoutNodes
    let secondTree = addAllNodes rightNodes WithoutNodes

    putStrLn (getLongestString firstTree ++ getLongestString secondTree)