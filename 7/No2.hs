import System.Random  
import Control.Monad.State  
import System.IO.Unsafe
--import Dsata.Foldable

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  

--instance Foldable Tree where
--    foldr f z EmptyTree = EmptyTree  
--    foldr f z (Node x leftsub rightsub) = f x (foldr f (foldr f z rightsub) leftsub)
    
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)          
    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

    
fromListT :: (Ord a) => [a] -> Tree a
fromListT nums = foldr treeInsert EmptyTree nums  

toListT :: (Ord a) => Tree a -> [a]
toListT EmptyTree = []
toListT (Node x left right) = toListT left ++ [x] ++ toListT right


randomizeT :: Tree Integer -> Tree Integer
randomizeT a = fmap (\_ -> unsafePerformIO $ getStdRandom (randomR (1,100))) a



--Example

--n = fromListT [1,6,3,5,12,3]
--toListT $ randomizeT n












