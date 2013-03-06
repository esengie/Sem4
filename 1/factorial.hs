module Main where

import System.IO
factorial :: Int -> Integer
factorial n = if (k == 1)
                then 1
                else k * (factorial (k - 1))
main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    putStrLn $ show $ factorial (read userInput :: Integer) 
