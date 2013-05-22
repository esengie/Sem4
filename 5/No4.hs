comb :: Int -> [Int]
comb l = do 
  k >>= \n -> k >>= \h -> return $ n * h 
  where k = [1..l]
