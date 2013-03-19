data Tree a = Nil
            | Branch a (Tree a) (Tree a)
foldTree :: (a -> a -> a) -> a -> (Tree a) -> a	
foldTree _ z Nil = z
foldTree f z (Branch v l r) = foldTree f (f v (foldTree f z l)) r 