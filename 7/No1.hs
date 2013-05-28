import qualified Data.Map as Map  
import Data.Char
import Data.Maybe
import System.IO
import Control.Monad.Error
import System.Environment  
import System.Directory 

data ReadError = BadFile | OtherError String  

instance Error ReadError where
  noMsg    = OtherError "A read Error!"
  strMsg s = OtherError s

instance Show ReadError where
  show BadFile = "Incorrect input"
  show (OtherError msg) = msg

--type MyMonad = Either ReadError

readin :: String -> IO (Map.Map String Integer)
readin src = do
    let p = map words (lines src)
    if all (\x->length x == 2) p
       then if all (\ (a:b:_) -> (reads b::[(Integer, String)]) /= []) p
          then do let pairs   = map (split.words) (lines src)
                  return (foldr insert Map.empty pairs)
          else do putStrLn "Not all are integers"
                  return Map.empty
       else do putStrLn "Incorrect format"
               return Map.empty 
    where
      insert (s, g) = Map.insert s g
      split [name,number] = (name, read number :: Integer)

toString :: Map.Map String Integer -> String
toString k = toString' (Map.toList k)

toString' :: [(String, Integer)] -> String
toString' [] = ""
toString' ((a,b):xs) = a ++ " " ++ show b ++ "\n" ++ (toString' xs)

main = do
    hSetBuffering stdin LineBuffering
    doLoop Map.empty
    
--reportResult :: MyMonad IO() -> IO ()
--reportResult (Right smth) = putStrLn ("Computations ended correctly")
--reportResult (Left e) = putStrLn ("Computations failed with error: " ++ (show e))
    
doLoop :: Map.Map String Integer -> IO()     
doLoop book = do    
    putStr "Enter:\n 0 - exit\n 1 - add\n 2 - lookup name\n 3 - lookup number\n 4 - save to file\n 5 - read from\n 6 - show\n"
    command <- getLine
    case command of
        "0"-> return ()
        "1" -> do 
                  putStrLn "Reading name "
                  nam <- getLine
                  putStrLn "Reading number "
                  num <- getLine    
                  if (reads num :: [(Integer,String)]) /= []
                    then doLoop (Map.insert nam (read num :: Integer) book)
                    else do putStrLn "Failed to read the number, care to try again?"
                            doLoop book
        "2" -> do 
                  putStrLn "Reading name"
                  nam <- getLine
                  putStr "Number:\n"
                  putStrLn $ show $ Map.findWithDefault 0 nam book
                  doLoop book
        "3" -> do
                  putStrLn "Reading number "
                  num' <- getLine
                  if (reads num' :: [(Integer,String)]) /= []
                      then do putStr $ "Name(s):\n"
                              putStrLn $ show.unwords.(map fst). Map.toList $ Map.filter (==  (read num' :: Integer))  book                              
                      else putStrLn "Failed to read the number, care to try again?"
                  doLoop book
        "5" -> do
                  putStr "Enter path\n"
                  x <- getLine
                  fileExists <- doesFileExist x  
                  if fileExists  
                    then do src <- readFile x
                            book' <- readin src  
                            doLoop book'
                    else do putStrLn "The file doesn't exist!"  
                            doLoop book
        "4" -> do
                  putStr "Enter name\n"
                  x <- getLine
                  writeFile x (toString book)
                  doLoop book
        "6" -> do
                  putStrLn $ show book
                  doLoop book
        _  -> do 
                  putStrLn "Error: No Such Command!"
                  doLoop book
        
        
        
        
        