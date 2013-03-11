import Data.List (elemIndex)
import Data.Maybe (fromJust)

findPos :: (Integral a) => [a] -> Int
findPos xs = fromJust (elemIndex (maximum sumList) sumList)
  where sumList = zipWith (+) (0 : xs) $ xs ++ [0]