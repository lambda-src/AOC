module Main where

import Day1
import Day2
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
        _ -> error "Haven't done the problem yet" 

