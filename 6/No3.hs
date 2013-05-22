import System.Random  
import Control.Monad.State  
import System.IO.Unsafe
import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  
    
height :: Tree a -> Int
height EmptyTree = 0
height (Node _ left right) = 1 + max (height left) (height right)

instance F.Foldable Tree where                                                -- таким образом получили поиск и размер бесплатно
    foldr f z EmptyTree = z  
    foldr f z (Node x leftsub rightsub) = f x (F.foldr f (F.foldr f z rightsub) leftsub)

removeElem :: (Ord a) => Tree a -> a -> Tree a                                -- тоже хак ибо лень                                   
removeElem t x = if (F.elem x t) then fromListT . filter (/=x) $ F.toList t
                             else t
          
    
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)          

fromListT :: (Ord a) => [a] -> Tree a
fromListT nums = foldr treeInsert EmptyTree nums  


randomizeT :: Tree Integer -> Tree Integer
randomizeT a = fmap (\_ -> unsafePerformIO $ getStdRandom (randomR (1,100))) a



--Example

--n = fromListT [1,6,3,5,12,3]
--F.toList $ randomizeT n












