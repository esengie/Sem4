import Control.Monad
import qualified Data.List as L

prod :: Int -> [Int] -> [[Int]]
prod 1 xs = L.group xs
prod n xs = liftM2 (:) xs $ prod (n-1) xs

prod2 :: Int -> Int -> [[Int]]
prod2 n 1 = prod 1 [1..n]
prod2 n m = (++) (prod m [1..n]) (prod2 n $ m-1)
  
tratata :: Int -> [[Int]]
tratata  n =  filter (\x -> n == sum x) me
  where me = prod2 n n