module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import System.Environment (getArgs)

main :: IO ()
main = do 
    (day : part : _) <- getArgs
    input <- lines <$> readFile ("inputs/Day" ++ day ++ ".txt")
    print $ case read day :: Int of 
        1 -> case read part :: Int of 
            1 -> doRotates input
            2 -> doRotateSteps input
            _ -> error "No part 3"
        2 -> case read part :: Int of
            1 -> doRepeat input 
            2 -> doRepeatWindow input
            _ -> error "No part 3"
        3 -> case read part :: Int of 
            1 -> doAllCombos input 
            _ -> error "Havent done it yet"
        4 -> case read part :: Int of 
            1 -> doNeighbors input 
            2 -> doRemoveNeighbors input
            _ -> error "No part 3"
        5 -> case read part :: Int of 
            1 -> doRanges input 
            _ -> error "Haven't finished it yet currently stuck"
        _ -> error "Haven't done the problem yet" 

