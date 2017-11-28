module Checkers where

--Type to describe what can be at each location on a checkers board.
data CPiece = White
           | WhiteKing
           | Black
           | BlackKing
     deriving (Eq, Show)

--Type to describe a checkers board
--The list is all the pieces on the board, where each inner list
--is a row (horizontal), and the outer lists is the list of all rows
type CBoard = [[Maybe CPiece]]

data CMove = Move (Int,Int) (Int, Int)
           | Take (Int,Int) (Int, Int)
    deriving (Eq, Show)


--CONSTRUCTING CHECKERS BOARDS

--Make an empty board (full of Nothing) of width w and
--height h
makeEmptyBoard :: Int -> Int -> CBoard
makeEmptyBoard w h = [[Nothing | x <- [1..w]] | y <- [1..h]]

--Make a standard board (8x8) populated with pieces in their
--starting positions.
makeStandardBoard :: CBoard
makeStandardBoard = board where 
    board = foldl (\x y -> setPiece x (Just Black) y) w_board b_positions
    w_board = foldl (\x y -> setPiece x (Just White) y) initial w_positions
    b_positions = [(x,y) | x <- [1..8], y <- [1,2,3], (x `mod` 2)/=(y `mod` 2)]
    w_positions = [(x,y) | x <- [1..8], y <- [6,7,8], (x `mod` 2)/=(y `mod` 2)]
    initial = makeEmptyBoard 8 8

--MOVING PIECES

--Execute a move, if it is possible.
move :: CBoard -> CMove -> Maybe CBoard
move b m@(Move (x1,y1) (x2,y2)) = if any (==m) (listMoves b (x1,y1))
    then Just (movePiece b m) else Nothing
move b m@(Take (x1,y1) (x2,y2)) = if any (==m) (listMoves b (x1,y1))
    then Just (movePiece b m) else Nothing

--Change a board by performing a given move.
movePiece :: CBoard -> CMove -> CBoard
movePiece b (Move (x1,y1) (x2,y2)) = setPiece b' p (x2,y2) where
    p = updatePiece b (x1, y1) (x2,y2)
    b' = setPiece b (Nothing) (x1,y1)
movePiece b (Take (x1,y1) (x2,y2)) = setPiece b'' p (x2,y2) where
    p = updatePiece b (x1, y1) (x2, y2)
    b'' = setPiece b' (Nothing) ((quot (x1+x2) 2), (quot (y1+y2) 2))
    b' = setPiece b (Nothing) (x1,y1)

--Get all the moves possible in a given space.
listMoves :: CBoard -> (Int,Int) -> [CMove]
listMoves b (x, y) = case getPiece b (x,y) of
    Just Black -> [Move(x,y) (x2,y2)|(x2,y2)<-allMoves b (x,y), y2>y]
        ++ [Take(x,y)(x2,y2)|(x2, y2)<-allTakes b (Just Black) (x, y)]
    Just White -> [Move(x,y) (x2,y2)|(x2,y2)<-allMoves b (x,y), y2<y]
        ++ [Take(x,y)(x2,y2)|(x2, y2)<-allTakes b (Just White) (x, y)]
    Just BlackKing -> [Move(x,y)(x2,y2)|(x2,y2)<-allMoves b (x,y)] ++
        [Take(x,y)(x2,y2)|(x2, y2)<-allTakes b (Just BlackKing) (x, y)]
    Just WhiteKing -> [Move(x,y)(x2,y2)|(x2,y2)<-allMoves b (x,y)] ++
        [Take(x,y)(x2,y2)|(x2, y2)<-allTakes b (Just WhiteKing) (x, y)]
    Nothing -> []

--GET/SET FUNCTIONS

--Get the piece at a given position.
getPiece :: CBoard -> (Int,Int) -> Maybe CPiece
getPiece board (x,y) = case (getElem board y) of 
    Nothing -> Nothing
    Just ls -> case (getElem ls x) of
        Nothing -> Nothing
        Just x  -> x

--Get the piece at a given position, updating it if necassary.
updatePiece :: CBoard -> (Int,Int) -> (Int,Int) -> Maybe CPiece
updatePiece b (x1,y1) (x2,y2) = case getPiece b (x1, y1) of
    Just Black ->if y2 == height b then Just BlackKing else Just Black
    Just White ->if y2 == 1 then Just WhiteKing else Just White
    a          -> a

--Set a piece in a given position, overwriting what is currently
--there. The types is:
--CBoard: the current state of the board,
--Maybe CPiece: The piece to insert (Nothing to remove a piece),
--(Int, Int): The x and y cgetoords of the position to insert at,
--CBoard: The board after insertion of the specified piece
setPiece :: CBoard -> Maybe CPiece -> (Int,Int) -> CBoard
setPiece [] _ (_,_) = []
setPiece ((x:l):ls) p (1,1) = (p : l) : ls
setPiece ((x:l):ls) p (w,1) = case (setPiece (l:ls) p ((w-1),1)) of
    (l2:ls2) -> (x:l2):ls2
    []       -> [[x]]
setPiece (l:ls) p (w,h)     = l : (setPiece ls p (w,(h-1)))


--MISC. FUNCTIONS

--Get the width of a board.
width :: CBoard -> Int
width []    = 0
width (h:t) = length h

--Get the height of a board.
height :: CBoard -> Int
height = length

--Get the n'th element from a list (if it exists).
getElem :: [a] -> Int -> Maybe a
getElem [] _ = Nothing
getElem (h:_) 1 = Just h
getElem (h:t) n = getElem t (n-1)

--Get all the open moves around a given spot:
allMoves :: CBoard -> (Int,Int) -> [(Int,Int)]
allMoves b (x,y) = [p|p<-onBoard, (getPiece b p) == Nothing] where
    onBoard = [(x,y) | (x,y)<-spaces, x<width b, y<height b, x>0, y>0]
    spaces = [(x+1,y+1), (x-1,y+1), (x+1,y-1),(x-1,y-1)]
    
--Get all the open 'Take' moves around a given piece in a given spot
allTakes :: CBoard -> Maybe CPiece -> (Int,Int) -> [(Int,Int)]
allTakes b Nothing _ = []
allTakes b (Just Black) (x,y) = takes where
    takes = [(x2,y2)|(x2,y2)<-free,
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just White ||
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just WhiteKing]
    free = [s | s <- onBoard, (getPiece b s) == Nothing]
    onBoard = [(x,y) | (x,y)<-spaces, x<width b, y<height b, x>0, y>0]
    spaces = [(x+2,y+2), (x-2,y+2)]
allTakes b (Just BlackKing) (x,y) = takes where
    takes = [(x2,y2)|(x2,y2)<-free,
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just White ||
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just WhiteKing]
    free = [s | s <- onBoard, (getPiece b s) == Nothing]
    onBoard = [(x,y) | (x,y)<-spaces, x<width b, y<height b, x>0, y>0]
    spaces = [(x+2,y+2), (x-2,y+2), (x+2,y-2), (x-2,y-2)]
allTakes b (Just White) (x,y) = takes where
    takes = [(x2,y2)|(x2,y2)<-free,
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just Black ||
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just BlackKing]
    free = [s | s <- onBoard, (getPiece b s) == Nothing]
    onBoard = [(x,y) | (x,y)<-spaces, x<width b, y<height b, x>0, y>0]
    spaces = [(x+2,y-2), (x-2,y-2)]
allTakes b (Just WhiteKing) (x,y) = takes where
    takes = [(x2,y2)|(x2,y2)<-free,
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just Black ||
        getPiece b (quot (x2+x) 2, quot (y2+y) 2) == Just BlackKing]
    free = [s | s <- onBoard, (getPiece b s) == Nothing]
    onBoard = [(x,y) | (x,y)<-spaces, x<width b, y<height b, x>0, y>0]
    spaces = [(x+2,y+2), (x-2,y+2), (x+2,y-2), (x-2,y-2)]

--Print a board
printBoard :: CBoard -> IO ()
printBoard b = putStr (showBoard b)

--Turn a board to a string
showBoard :: CBoard -> String
showBoard [] = []
showBoard (r:b) = (foldr (\x y -> showPiece x ++ y) [] r )++ "\n" ++ showBoard b

--Turn a Maybe CPiece into a string
showPiece :: Maybe CPiece -> String
showPiece (Just White) = "w"
showPiece (Just WhiteKing) = "W"
showPiece (Just Black) = "b"
showPiece (Just BlackKing) = "B"
showPiece (Nothing) = "-"
