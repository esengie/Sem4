import Control.Monad.Cont

cMap :: (a -> b) -> [a] ->  Cont [b] [b]
cMap _ [] = return []
cMap f (x:xs) = cMap f xs >>= return . (f x :)


map' f xs = runCont (cMap f xs) id

cFilter :: (a -> Bool) -> [a] ->  Cont [a] [a]
cFilter _ [] = return []
cFilter f (x:xs) = cFilter f xs >>= return . (filter f [x]!!0 :)

filter' f xs = runCont (cFilter f xss) id