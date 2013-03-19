module Main where

import GHC.IO

   
addTo :: (Ord a) => [a] -> a -> [a]
addTo [] var = [var]
addTo x var | head(x) == var = x
	    | head(x) > var = (var:x)
	    | otherwise = (head(x) : addTo (tail x) var)
	    
	    				   
removeFrom :: (Ord a) => [a] -> a -> [a]
removeFrom [] var = []
removeFrom x var | head(x) == var = tail(x)
		 | otherwise = (head(x): removeFrom (tail x) var)
   
  

doLoop = do
	    let listt = []
	    putStrLn "Enter a command \n0 - exit \n1 - add value to sorted list \n2 - remove value from list \n3 - print list:"
	    command <- getLine
	    case command of
			    '0': _ ->   return ()
			    '1': var -> do  
					    print  (let listt = (addTo listt var))
					      doLoop
			    '2': var -> do 
					    print (let listt = (removeFrom listt var))
					    doLoop
			    '3': _ ->   do print listt
					      doLoop
                               
main = do
	  hSetBuffering stdin LineBuffering
	  doLoop