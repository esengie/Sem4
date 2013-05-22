import Control.Monad
import qualified Data.List as L

prod :: Int -> [[Int]]
prod n = prod' n [1,2,3]

prod' :: Int -> [Int] -> [[Int]]
prod' 1 xs = L.group xs
prod' n xs = liftM2 (:) xs $ prod' (n-1) xs