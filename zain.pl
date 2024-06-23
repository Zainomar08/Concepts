type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)
createBoard :: Position -> Board
createBoard position
    | isvalidposition position = [ Peg (x, y) (if (x, y) == position then W else B) 
	                        | x <- [-3..3], y <- [-3..3], isvalidposition (x, y) ]
    | otherwise = error "The position is not valid."
    where
        isvalidposition (x, y) = abs x < 2 || abs y < 2

surronding :: Position -> [Position]
surronding (x, y) = filter isvalidposition [(x + xc, y + yc) | (xc, yc) <- pn]
    where
        pn = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        isvalidposition (xc, yc) = abs xc < 2 || abs yc < 2


isValidMove :: Move -> Board -> Bool
isValidMove (M pos) board =
    any (\peg -> isBlack peg pos) board && any (\p -> isWhite p board) (surronding pos)
    where
        isBlack (Peg p c) p2= p == p2 && c == B
        isWhite p board = any (\(Peg p1 c) -> p1 == p && c == W) board

isGoal :: Board -> Bool
isGoal [] = True
isGoal (Peg p c : xr) = c == W && isGoal xr





showPossibleNextStates :: Board -> [State]
showPossibleNextStates board
    | isGoal board = error "No Possible States Exist."
    | otherwise = generateNextStates board
    where 
        generateNextStates [] = []
        generateNextStates (Peg pos color : xs)
            | color == B = if isValidMove (M pos) board
                                then S (M pos) (changecolortoW pos board) : generateNextStates xs
                                else generateNextStates xs
            | otherwise = generateNextStates xs
        
        
        changecolortoW _ [] = []
        changecolortoW pos (Peg p c : xs)
                       | p == pos  = Peg p W : changecolortoW pos xs
                       | otherwise = Peg p c : changecolortoW pos xs