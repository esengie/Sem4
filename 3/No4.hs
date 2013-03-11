data BinTree a = EmptyBinTree
                  | BinTreeNode a (BinTree a) (BinTree a)


binTreeFromList :: (Ord) a => [a] -> BinTree a
binTreeFromList = foldl insert EmptyBinTree
    where insert EmptyBinTree                    x             = BinTreeNode x EmptyBinTree EmptyBinTree
          insert node@(BinTreeNode root left right) x | x < root     = BinTreeNode root (insert left x) right
                                                      | x > root     = BinTreeNode root left (insert right x)
                                                      | otherwise = node

                  
                  
binTreeHeight :: BinTree a -> Int
binTreeHeight EmptyBinTree                  = 0
binTreeHeight (BinTreeNode root left right) = 1 + max (binTreeHeight left) (binTreeHeight right)