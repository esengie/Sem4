{-# LANGUAGE FlexibleInstances #-}

import Data.Array(Array(..), array, bounds, elems, (//), (!))
import Data.List(foldl')
import Data.Char
import Control.Monad.State

class HashTranform a where
    hashPrepare :: a -> Integer
 
instance HashTranform Integer where
    hashPrepare = id 
 
instance HashTranform String where
    hashPrepare cs = fromIntegral (foldl' (flip ((+) . ord)) 0 cs)
 
 
divHashForSize :: (HashTranform a) => Integer -> a -> Integer
divHashForSize sz k = 1 + (hashPrepare k) `mod` sz 

 
type Chain k v = [(k, v)]

chainWith :: (Eq k) => Chain k v -> (k, v) -> Chain k v
chainWith cs p@(k, v) = if (null after) then p:cs else before ++ p:(tail after)
  where (before, after) = break ((== k) . fst) cs
 
 
chainWithout :: (Eq k) => Chain k v -> k -> Chain k v
chainWithout cs k = filter ((/= k) . fst) cs 
 
 
data Hash k v = Hash {
    hashFunc   :: (k -> Integer)
  , chainTable :: Array Integer (Chain k v) 
} 
 
--type HState k v = State (Hash k v)
 
instance (Show k, Show v) => Show (Hash k v) where
    show = show . concat . elems . chainTable
 
 
type HashFuncForSize k = Integer -> k -> Integer
 
createHash :: HashFuncForSize k -> Integer -> Hash k v
createHash hs sz = Hash (hs sz) (array (1, sz) [(i, []) | i <- [1..sz]])
 
 
withSlot :: Hash k v -> k -> (Chain k v -> Chain k v) -> Hash k v
withSlot h k op
    | rows < hashed = h
    | otherwise     = Hash hf (ht // [(hashed, op (ht!hashed))])
  where hf     = hashFunc h
        ht     = chainTable h
        rows   = snd (bounds ht) 
        hashed = hf k
search :: (Eq k) => k -> Hash k v -> Maybe v
search k h
    | rows < hashed = Nothing
    | otherwise     = k `lookup` (ht!hashed) 
  where hf     = hashFunc h
        ht     = chainTable h
        rows   = snd (bounds ht)
        hashed = hf k
 
insert' :: (Eq k) => Hash k v -> (k, v) -> Hash k v
insert' h p@(k, v) = withSlot h k (flip chainWith p)
 
 
delete' :: (Eq k) => Hash k v -> k -> Hash k v
delete' h k = withSlot h k (flip chainWithout k)
 
 
insert :: (Eq k) => Hash k v -> Chain k v -> Hash k v
insert src pairs = foldl' insert' src pairs
 
 
delete :: (Eq k) => Hash k v -> [k] -> Hash k v
delete src keys = foldl' delete' src keys

deleteEr :: (Eq k) => [k] -> State (Hash k v) ()
deleteEr k = modify $ flip delete k

insertEr :: (Eq k) => Chain k v -> State (Hash k v) ()
insertEr pairs = modify $ flip insert pairs


-- let intHash = (createHash (divHashForSize :: (Integer -> Integer -> Integer)) 10)
-- runState (insertEr  [(1112, "a"), (211, "b")] >> deleteEr [1112, 211]) intHash 











   
 
