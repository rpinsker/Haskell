--lab4.hs


module Main where

import Data.Char
import Data.List
--Extra Credit 1
main :: IO ()
main = do
  input <- getLine
  putStrLn (show (eval (simpleParseExpr input)))


data ArithExpr = Number Integer
                 | Plus ArithExpr ArithExpr
                 | Mult ArithExpr ArithExpr
                 deriving (Show)  
                          
eval :: ArithExpr -> Integer
eval (Number expr) = expr              
eval (Plus expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mult expr1 expr2) =  (eval expr1) * (eval expr2) 

simpleParseExpr :: String -> ArithExpr
simpleParseExpr str 
    | (tokenise (deleteWhitespace str)) == [] =  error "Bad Expression"
    | (length (tokenise (deleteWhitespace str))) == 1 = (Number (read (head (tokenise (deleteWhitespace str)))))
    | "+" `elem` (tokenise (deleteWhitespace str))  =  Plus (simpleParseExpr (concat (fst (break (== "+") (tokenise (deleteWhitespace str)))))) (simpleParseExpr (concat (tail (snd (break (== "+") (tokenise (deleteWhitespace str)))))))
    | "*" `elem` (tokenise (deleteWhitespace str))  =  Mult (simpleParseExpr (concat (fst (break (== "*") (tokenise (deleteWhitespace str)))))) (simpleParseExpr (concat (tail (snd (break (== "*") (tokenise (deleteWhitespace str)))))))

deleteWhitespace :: String -> String
deleteWhitespace str 
    | str == "" = ""
    | (isSpace (head str)) = deleteWhitespace (tail str)             
    | otherwise = (head str) : deleteWhitespace (tail str)                  

tokenise :: String -> [String]
tokenise "" = []
tokenise (x:xs) 
  | isDigit x = (fst (span isDigit (x:xs))) : (tokenise (snd (span isDigit (x:xs))))
  | x == '+' =  ['+'] : (tokenise xs)            
  | x == '*' =  ['*'] : (tokenise xs)             
  | x == '-' =  ((++) ['-'] (fst (span isDigit xs))) : (tokenise (snd (span isDigit xs)))        
  | x == '(' && (length (elemIndices ')' xs)) == 1 = (takeWhile (/= ')') xs) : (tokenise (tail (dropWhile (/= ')') xs))) 
{- I tried to do extra credit two, but it currently only works if there is one set of parentheses. I can't figure out how to find the correct ')' that goes with the '(' but here is the beginning of my attempt.  
  | x == '(' = (++) (handleParens xs (length (elemIndices '(' (takeWhile (/= ')') xs)))) (tokenise (tail (dropWhile (/= ')') xs))) 
-}  
  | otherwise = []           
                
{-
--handleParens :: String -> Int -> String
handleParens xs n
  | n == 0 = (++) (takeWhile (/= ')') xs) ""
  | otherwise = (++) ((++) (takeWhile (/= ')') xs) ")") (handleParens (tail (dropWhile (/= ')') xs)) (n-1))  
-}
                
--Extra Credit 2
--Note: This doesn't work for nested parentheses. I can't figure out how to check if the closest ')' to the '(' is the one that goes with the '('
parseExpr :: String -> ArithExpr
parseExpr str 
    | (tokenise (deleteWhitespace str)) == [] =  error "Bad Expression"
    |  (length (tokenise (deleteWhitespace str))) == 1 && (length (head (tokenise (deleteWhitespace str)))) == 1 = (Number (read (head (tokenise (deleteWhitespace str)))))
    | "+" `elem` (tokenise (deleteWhitespace str))  =  Plus (simpleParseExpr (concat (fst (break (== "+") (tokenise (deleteWhitespace str)))))) (simpleParseExpr (concat (tail (snd (break (== "+") (tokenise (deleteWhitespace str)))))))
    | "*" `elem` (tokenise (deleteWhitespace str))  =  Mult (simpleParseExpr (concat (fst (break (== "*") (tokenise (deleteWhitespace str)))))) (simpleParseExpr (concat (tail (snd (break (== "*") (tokenise (deleteWhitespace str)))))))
    | otherwise = parseExpr (head (tokenise (deleteWhitespace str)))           
                
                
  
-- Tests for eval and simpleParseExpr (deleteWhitespace and tokenise are used by simpleParseExpr, so they are tested when simpleParseExpr is tested)
test1 = eval (Number 2) == 2 
test2 = eval (Mult (Plus (Number 3) (Number 4)) (Number 5)) == 35
test3 = (show (simpleParseExpr "1 + 2 * 3")) == "Plus (Number 1) (Mult (Number 2) (Number 3))"
test4 = (eval (simpleParseExpr "1 + 2")) == 3
test5 = (eval (simpleParseExpr "2 * -3")) == (-6)
test = test1 && test2 && test3  && test4 && test5