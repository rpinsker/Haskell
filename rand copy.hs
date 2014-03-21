module Rand where

import RandState
import System.Random
import UCState
import Data.List
import Control.Monad 

randR :: Random a => (a,a) -> RandState a
randR (a,b) = do
    gen <- get
    let (x, gen') = randomR (a,b) gen
    put gen'
    return x 
    
    
rollTwoDice :: RandState Int   
rollTwoDice = do
  a <- randR (1,6)
  b <- randR (1,6)
  return (a + b)
  
-- Data types to represent playing cards
data CardValue = King | Queen | Jack | NumberCard Int
    deriving (Show, Eq)
data CardSuit = Hearts | Diamonds | Spades | Clubs
    deriving (Show, Eq)
data PlayingCard = PlayingCard CardSuit CardValue
    deriving (Show, Eq)

{-
 - fullCardDeck will be a deck of cards, 52 in total, with a King, a Queen, 
 - a Jack and NumberCards from 1 to 10 for each suit.
 -}
fullCardDeck :: [PlayingCard]
fullCardDeck = [ PlayingCard s v | s <- allsuits, v <- allvals ] where
        allvals = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allsuits = [Hearts, Diamonds, Spades, Clubs]  
        
removeCard :: [PlayingCard] -> RandState (PlayingCard,[PlayingCard])       
removeCard listOfCards = do
  randIndex <- randR (0,((length listOfCards) - 1)) 
  let card = (!!) listOfCards randIndex
  let newList = delete card listOfCards
  return (card,newList)
  
shuffleDeck :: [PlayingCard] -> RandState [PlayingCard]
shuffleDeck (x:xs) = do
  case (length (x:xs)) of
    1  -> return [x]
    _ -> do 
      (card, newList) <- removeCard (x:xs)
      x <- shuffleDeck newList
      return $ card : x 
      
testShuffleDeck :: IO [PlayingCard]     
testShuffleDeck = do 
    -- create new random number generator
    gen <- newStdGen 
    -- run rand with the new random number generator.  The fact that the
    --  type of stateExample1 is IO Int ensures that rand is forced to generate
    --  a random value of type Int
    let r = runRandom (shuffleDeck fullCardDeck) gen 
    return r
                    
testRollTwoDice :: IO Int
testRollTwoDice = do
  gen <- newStdGen
  let r = runRandom rollTwoDice gen
  return r
{- Running testRollTwoDice a few times to test rollTwoDice. Since rollTwoDice uses randR, this also tests randR  
*Rand> testRollTwoDice
7
*Rand> testRollTwoDice 
7
*Rand> testRollTwoDice 
6
*Rand> testRollTwoDice 
12
*Rand> testRollTwoDice 
7
*Rand> testRollTwoDice 
5
*Rand> testRollTwoDice 
9
*Rand> testRollTwoDice 
7
*Rand> testRollTwoDice 
5
*Rand> testRollTwoDice 
5
-}
  
  
--test removeCard
testRemoveCard :: IO (PlayingCard, [PlayingCard])
testRemoveCard = do
  let deck = [(PlayingCard (Diamonds) (Queen)),(PlayingCard (Spades) (King)),(PlayingCard Hearts Queen),(PlayingCard Hearts King)]
  gen <- newStdGen
  let r = runRandom (removeCard deck) gen
  return r

{- Running testRemoveCard a few times to see that it removes a card randomly
*Rand> testRemoveCard
(PlayingCard Diamonds Queen,[PlayingCard Spades King,PlayingCard Hearts Queen,PlayingCard Hearts King])
*Rand> testRemoveCard
(PlayingCard Diamonds Queen,[PlayingCard Spades King,PlayingCard Hearts Queen,PlayingCard Hearts King])
*Rand> testRemoveCard
(PlayingCard Hearts Queen,[PlayingCard Diamonds Queen,PlayingCard Spades King,PlayingCard Hearts King])
*Rand> testRemoveCard
(PlayingCard Spades King,[PlayingCard Diamonds Queen,PlayingCard Hearts Queen,PlayingCard Hearts King])
*Rand> testRemoveCard
(PlayingCard Diamonds Queen,[PlayingCard Spades King,PlayingCard Hearts Queen,PlayingCard Hearts King])
-}  
  
  
{---------------------------------------------
Extra Credit
---------------------------------------------}  
  
-- Succeed if randomly chosen point from square is inside circumscribed circle 
piTrial :: RandState Bool
piTrial = do
  x <- randR ((-1.0),1.0) :: RandState Double
  y <- randR ((-1.0),1.0) :: RandState Double
--  let dist = sqrt((x*x) + (y*y))
  return ((sqrt((x^2)+(y^2))) < 1.0)    
  
testPiTrial :: IO Bool  
testPiTrial = do  
  gen <- newStdGen
  let r = runRandom (piTrial) gen
  return r
  
{- Running testPiTrial a few times to see that it works
*Rand> testPiTrial
False
*Rand> testPiTrial
False
*Rand> testPiTrial
True
*Rand> testPiTrial
True
*Rand> testPiTrial
True
*Rand> testPiTrial
True
*Rand> testPiTrial
False
-}

-- Perform n trials of the RandState function provided as the second argument,
--  and give back the number of successful trials
-- Hint: To perform the n trials, you can either use sequence from 
--       Control.Monad, or you can use recursion 
bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials n fxn = do
  gen <- get
  randR (0,1) :: RandState Int
  let a = runRandom piTrial gen
  if n == 0 
    then do return 0
    else if a 
         then do 
           x <- bernoulliTrials (n-1) fxn :: RandState Int
           return $ 1 + x
         else do   
           x <- bernoulliTrials (n-1) fxn
           return x
 
testBernoulliTrials :: IO Int 
testBernoulliTrials = do
  gen <- newStdGen
  let r = runRandom (bernoulliTrials 10000 piTrial) gen
  return r
  
{- Running testBernoulliTrials a few times   
*Rand> testBernoulliTrials 
7857
*Rand> testBernoulliTrials 
7735
*Rand> testBernoulliTrials 
7757
*Rand> testBernoulliTrials 
7795
-}


-- Approximate pi using n randomly chosen points
-- Hint: You will probably need to use the fromIntegral function to
--       convert Int into Double.
approxPi :: Int -> RandState Double
approxPi n = do 
  x <- bernoulliTrials n piTrial 
  let z = (fromIntegral x/fromIntegral n)
  return $ 4 * z
  
approxPi':: Int -> IO Double
approxPi' n = do
  gen <- newStdGen
  let r = runRandom (approxPi n) gen
  return r
  
{- Running approxPi' a few times to test both approxPi' and approxPi  
*Rand> approxPi' 100000 
3.14392
*Rand> approxPi' 100000 
3.14816
*Rand> approxPi' 100000 
3.13848
*Rand> approxPi' 100000 
3.13744
-}