module Main where

import System.IO

main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    let factorial = \ n ->
                        \ fact ->
                            fact fact n
                        \ (ft, k) ->
                            if (k == 1)
                               then 1
                               else k * (ft ft (k - 1))

    putStrLn $ show $ factorial (read userInput :: Integer) 
