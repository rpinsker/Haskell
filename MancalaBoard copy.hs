module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- uncomment these type declarations and implement the functions

{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player

-- CHECK: replace below line with real definition
getCurPlayer (MancalaBoardImpl xs p) = p


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
-- CHECK: replace below line with real definition
getBoardData (MancalaBoardImpl xs p) = (drop (indexForFirstPit p) . take (indexForPlayerStore p) $ xs) ++ (drop (indexForFirstPit (nextPlayer p)) . take (indexForPlayerStore (nextPlayer p)) $ xs)
  --indicesForPlayerSide PlayerA ++ indicesForPlayerSide PlayerB

{- return the side of the board for a specified player, including the store at
 - the end -}
-- CHECK: define this function
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl xs curPlayer) p = (drop (indexForFirstPit p) . take ((indexForPlayerStore p) + 1) $ xs)

{- return the number of captured pieces in specified player's store -}
-- CHECK: add type and replace below line with real definition
numCaptured :: MancalaBoard -> Player -> Int
numCaptured (MancalaBoardImpl xs player) p = xs !! indexForPlayerStore p


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
-- CHECK: add type and replace below line with real definition
allowedMoves :: MancalaBoard -> Player -> [Int]
allowedMoves (MancalaBoardImpl xs player) p = filter (\x -> (xs !! x) > 0) (indicesForPlayerSide p)

{- check that a move is valid for the current player -}
-- CHECK: add type and replace below line with real definition
isAllowedMove :: Int -> MancalaBoard -> Bool
isAllowedMove x (MancalaBoardImpl xs p) = x `elem` (allowedMoves (MancalaBoardImpl xs p) p) 


{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
--TODO: add type and replace below line with real definition
move :: MancalaBoard -> Int -> MancalaBoard
move (MancalaBoardImpl xs p) i = (MancalaBoardImpl newXS (nextPlayer p)) where
  countStones = (xs !! i) 
  pickUpPit = ((init (fst (splitAt (i+1) xs))) ++ [0]) ++ (snd (splitAt(i+1) xs))
  newXS =  distribute countStones (MancalaBoardImpl pickUpPit p) (i+1)
  distribute count (MancalaBoardImpl ys player) index 
    | count == 0 = ys
    | index > ((length ys) - 1) = distribute count (MancalaBoardImpl ys player) 0
    | index /= (indexForPlayerStore (nextPlayer player)) = distribute (count - 1) (MancalaBoardImpl (replace ys index) player) (index + 1) 
    | index == (indexForPlayerStore (nextPlayer player)) = distribute count (MancalaBoardImpl ys player) (index + 1) 
                                                                      
attemptMove (MancalaBoardImpl xs p) i = if isAllowedMove i (MancalaBoardImpl xs p) 
                 then move (MancalaBoardImpl xs p) i
                 else MancalaBoardImpl xs p     

replace :: [Int] -> Int -> [Int]      
replace xs i = (init (fst (splitAt (i+1) xs))) ++ (((last (fst (splitAt (i+1) xs))) + 1) :[])   ++ (snd (splitAt(i+1) xs))

{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- CHECK: replace below line with real definition
gameOver (MancalaBoardImpl xs p) 
  | all (==0) (init (playerSide (MancalaBoardImpl xs p) PlayerA)) || all (==0) (init (playerSide (MancalaBoardImpl xs p) PlayerB)) = True
  | otherwise = False                                                                                                                     


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners (MancalaBoardImpl xs p)   
  | numCaptured (MancalaBoardImpl xs p) PlayerA == numCaptured (MancalaBoardImpl xs p) PlayerB = []
  | numCaptured (MancalaBoardImpl xs p) PlayerA > numCaptured (MancalaBoardImpl xs p) PlayerB = [PlayerA]                        | otherwise = [PlayerB]                                                                

--show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) = (show (playerSide (MancalaBoardImpl boardData player) PlayerA)) ++ " \n" ++ (show (playerSide (MancalaBoardImpl boardData (nextPlayer player)) PlayerB)) ++ " \n" ++ show player

--Tests
testGetCurPlayer :: Bool    
testGetCurPlayer = (getCurPlayer initial == PlayerA) && (getCurPlayer initial /= PlayerB)

testGetBoardData :: Bool
testGetBoardData = (getBoardData initial == [4,4,4,4,4,4,4,4,4,4,4,4])

testPlayerSide :: Bool
testPlayerSide = (playerSide initial PlayerA == [4,4,4,4,4,4,0]) && (playerSide initial PlayerB /= [1,2,3,4,5,6,7])

testNumCaptured :: Bool
testNumCaptured = numCaptured initial PlayerA == 0 && numCaptured (MancalaBoardImpl [0,0,0,0,0,0,0,0,0,0,0,0,0,23] PlayerA) PlayerB == 23

testAllowedMoves :: Bool
testAllowedMoves = allowedMoves initial PlayerA == [0,1,2,3,4,5] && allowedMoves (MancalaBoardImpl [0,0,0,0,0,0,0,0,0,0,0,0,2,0] PlayerA) PlayerB == [12]

testIsAllowedMove :: Bool
testIsAllowedMove = isAllowedMove 2 initial && not (isAllowedMove 3  (MancalaBoardImpl [0,0,0,0,0,0,0,0,0,0,0,0,2,0] PlayerA)) &&  isAllowedMove 12  (MancalaBoardImpl [0,0,0,0,0,0,0,0,0,0,0,0,2,0] PlayerB)

testMove :: Bool 
testMove = show (attemptMove initial 1) == show (MancalaBoardImpl [4,0,5,5,5,5,0,4,4,4,4,4,4,0] PlayerB) && show (attemptMove initial 12) == show (MancalaBoardImpl [4,4,4,4,4,4,0,4,4,4,4,4,4,0] PlayerA)

testGameOver :: Bool 
testGameOver = gameOver (MancalaBoardImpl [0,0,0,0,0,0,10,0,0,0,0,0,1,20] PlayerA) && not (gameOver (MancalaBoardImpl [0,0,0,3,0,0,10,0,0,0,0,0,1,20] PlayerA)) && gameOver (MancalaBoardImpl [0,0,0,0,4,5,10,0,0,0,0,0,0,20] PlayerA) && not (gameOver initial)

testWinners :: Bool
testWinners = winners initial == [] &&  winners (MancalaBoardImpl [0,0,0,0,4,5,10,0,0,0,0,0,0,20] PlayerA) == [PlayerB] &&  winners (MancalaBoardImpl [0,0,0,0,4,5,10,0,0,0,0,0,0,5] PlayerA) == [PlayerA]

testAll :: Bool 
testAll = testGetCurPlayer && testGetBoardData && testPlayerSide && testNumCaptured && testAllowedMoves && testIsAllowedMove && testGameOver && testWinners && testMove