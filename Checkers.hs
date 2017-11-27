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


--CONSTRUCTING CHECKERS BOARDS

--Make an empty board (full of Nothing) of width w and
--height h
makeEmptyBoard :: Int -> Int -> CBoard
makeEmptyBoard w h = [[Nothing | x <- [1..w]] | y <- [1..h]]


--GET/SET FUNCTIONS

--Get the piece at a given position.
getPiece :: CBoard -> Int -> Int -> Maybe CPiece
getPiece board x y = case (getElem board y) of 
    Nothing -> Nothing
    Just ls -> case (getElem ls x) of
        Nothing -> Nothing
        Just x  -> x

--Set a piece in a given position, overwriting what is currently
--there. The types is:
--CBoard: the current state of the board,
--Maybe CPiece: The piece to insert (Nothing to remove a piece),
--Int: The x coord of the position to insert at,
--Int: The y coord of the position to insert at.
--CBoard: The board after insertion of the specified piece
setPiece :: CBoard -> Maybe CPiece -> Int -> Int -> CBoard
setPiece [] _ _ _ = []
setPiece ((x:l):ls) p 1 1 = (p : l) : ls
setPiece ((x:l):ls) p w 1 = case (setPiece (l:ls) p (w-1) 1) of
    (l2:ls2) -> (x:l2):ls2
    []       -> [[x]]
setPiece (l:ls) p w h     = l : (setPiece ls p w (h-1))


--MISC. FUNCTIONS

--Get the n'th element from a list (if it exists).
getElem :: [a] -> Int -> Maybe a
getElem [] _ = Nothing
getElem (h:_) 0 = Just h
getElem (h:t) n = getElem t (n-1)
