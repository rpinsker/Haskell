import MancalaBoard
import Data.List

main :: IO ()
main = do
  putStrLn ("Welcome to Mancala")
  putStrLn ("To play, when it is your turn enter the number of the pit from which you would like to move the stones. The prompt will provide you with a list of valid moves. Once one side is empty, the game will end and the winner will be printed.")
  putStrLn ("To quit, type -1 instead of a pit to move to. \n")
  putStrLn ("Please enter the number of players to begin. (1 or 2)")
  args <- getLine
  let num = read args
  case num of 
    1 -> playGameAIPlayerMove initial
    2 -> playGame initial
    _ -> do
      putStrLn ("Please try again and enter 1 or 2. The game will now reset.")
      main

playGameAI :: MancalaBoard -> IO ()
playGameAI board = do
  putStrLn (show board) 
  let num = nextMove board
  putStrLn ("AI is moving pit: " ++ (show num))
  let newBoard = move board num
  case (gameOver newBoard) of    
    True -> endGame newBoard
    False -> playGameAIPlayerMove newBoard
    
nextMove :: MancalaBoard -> Int
nextMove board = lookAhead board 0

evalCurPos :: Player -> Int -> MancalaBoard -> Int
evalCurPos p 0 board = heuristicScore p board
evalCurPos p int board = lookAhead (move board (nextMove board)) (int-1)

lookAhead :: MancalaBoard -> Int -> Int
lookAhead board int = num where
         boards = map (move board) (allowedMoves board (getCurPlayer board)) 
         scores = map (evalCurPos (getCurPlayer (move board 0)) int) boards
         (Just index) = elemIndex (maximum scores) scores
         num = (allowedMoves board (getCurPlayer board)) !! index

--Returns the difference of stones in the player's store compared to the other player's store
heuristicScore :: Player -> MancalaBoard -> Int
heuristicScore p board = (numCaptured board (getCurPlayer (move board 7))) - (numCaptured board p)

playGameAIPlayerMove :: MancalaBoard -> IO ()
playGameAIPlayerMove board = do
  putStrLn (show board)
  putStrLn ("\n" ++ (show (getCurPlayer board)) ++ ", please enter the pit number you would like to move stones from. Allowed moves include: " ++ (show (allowedMoves board (getCurPlayer board)))) 
  args <- getLine
  let x = read args
  case x of      
    (-1) -> putStrLn ("Quit.")
    _ -> case (isAllowedMove x board) of
      True -> do
        let newBoard = move board x
        case (gameOver newBoard) of
          True -> endGame newBoard
          False -> playGameAI newBoard
      False -> do 
        putStrLn ("Please try again with a valid move.")
        playGameAIPlayerMove board  

  

playGame :: MancalaBoard -> IO ()
playGame board = do
  putStrLn (show board)
  putStrLn ("\n" ++ (show (getCurPlayer board)) ++ ", please enter the pit number you would like to move stones from. Allowed moves include: " ++ (show (allowedMoves board (getCurPlayer board)))) 
  args <- getLine
  let x = read args
  case x of      
    (-1) -> putStrLn ("Quit.")
    _ -> case (isAllowedMove x board) of
      True -> do
        let newBoard = move board x
       -- putStrLn (show newBoard)            
        case (gameOver newBoard) of
          True -> endGame newBoard
          False -> playGame newBoard
      False -> do 
        putStrLn ("Please try again with a valid move.")
        playGame board  
  
  
endGame :: MancalaBoard -> IO ()
endGame board = do
  let xs = winners board
  case xs of    
    [] -> putStrLn ("The game is a draw.")
    x:_ -> putStrLn ((show x) ++ " is the winner.")