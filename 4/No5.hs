module Main where

import GHC.IO
import Data.Char
   
addTo :: [Int] -> Int -> [Int]
addTo [] var = [var]
addTo x var | head(x) == var = x
	    | head(x) > var =  (var:x)
	    | otherwise = (head(x) : addTo (tail x) var)
	    
	    				   
removeFrom :: [Int] -> Int -> [Int]
removeFrom [] var = [] 
removeFrom x var | head(x) == var = tail(x)
		 | otherwise = (head(x): removeFrom (tail x) var)
   
  

doLoop listt = do
	    
	    putStrLn "Enter a command \n0 - exit \n1 - add value to sorted list \n2 - remove value from list \n3 - print list:"
	    command <- getLine
	    case command of
			    '0': _ ->   return ()
			    '1': _ -> do  var <- getLine
					  listt <- return (addTo listt (digitToInt $ head $ tail (show var))):: IO[Int]
					  print  listt
					  (doLoop listt)
			    '2': _ -> do var <- getLine
					 listt <- return (removeFrom listt (digitToInt $ head $ tail  (show var))) :: IO[Int]
					 print listt
					 (doLoop listt)
			    '3': _ ->   do 
					   print listt
					   (doLoop listt)
			     
                               
main = do
	 -- hSetBuffering stdin LineBuffering
	 listt <- return [] :: IO [Int]
	 doLoop listt