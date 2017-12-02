import Checkers
import Data.IORef

blankline :: IO ()
blankline = putStrLn " "

gameStart :: IO ()
gameStart = do
   putStrLn "Game Start!, White is first to move"
   printBoard makeStandardBoard

recurse :: CBoard -> IO b
recurse board = do
   board2 <- blackMove board
   board3 <- whiteMove board2
   board <- blackMove board3
   board2 <- whiteMove board
   recurse board2

   
validMove :: CBoard -> Int -> Int -> IO CBoard
validMove b x y = do
   blankline
   putStrLn "Where would you like to move?"
   putStrLn "x-coord? (1-8)"
   xin <- getLine
   putStrLn "y-coord? (1-8)"
   yin <- getLine

   let x2 = (read xin :: Int)
   let y2 = (read yin :: Int)

   let newb = (movePiece b (Take (x,y) (x2,y2)))
   printBoard newb
   return newb   

whiteMove :: CBoard -> IO CBoard
whiteMove board = do
   blankline
   putStrLn "WHITE's Turn: "
   let b = board
   putStrLn "Which piece would you like to move?"
   putStrLn "x-coord? (1-8)"
   xin <- getLine
   putStrLn "y-coord? (1-8)"
   yin <- getLine
   let x = (read xin :: Int)
   let y = (read yin :: Int)

   putStrLn "Where would you like to move it to? Valid moves are..."
   let list = listMoves b (x,y)
   print list 
   validMove b x y 
 

blackMove :: CBoard -> IO CBoard
blackMove board = do
   blankline
   putStrLn "BLACK's Turn: "
   let b = board
   putStrLn "Which piece would you like to move?"
   putStrLn "x-coord? (1-8)"
   xin <- getLine
   putStrLn "y-coord? (1-8)"
   yin <- getLine
   let x = (read xin :: Int)
   let y = (read yin :: Int)

   putStrLn "Where would you like to move it to? Valid moves are..."
   let list = listMoves b (x,y)
   print list 
   validMove b x y 


--Call Main to Start the Game 
   --assuming always correct inputs and know the axis system
   --3645433476656354654327368374837263455463453654344574655634

--win condition
--move/take case
--invalid coord recurse

main = do
  
   gameStart
   board <- whiteMove makeStandardBoard
   recurse board
   putStrLn "End"
  
 