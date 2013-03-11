data BinTree a = EmptyBinTree
                  | BinTreeNode a (BinTree a) (BinTree a)


binTreeFromList :: (Ord) a => [a] -> BinTree a
binTreeFromList = foldl insert EmptyBinTree
    where insert EmptyBinTree                    val             = BinTreeNode val EmptyBinTree EmptyBinTree
          insert node@(BinTreeNode root left right) val | val < root     = BinTreeNode root (insert left val) right
                                                      | val > root     = BinTreeNode root left (insert right val)
                                                      | otherwise = node

                  
                  
binTreeMinHeight :: BinTree a -> Int
binTreeMinHeight EmptyBinTree                  = 0
binTreeMinHeight (BinTreeNode root left right) = 1 + min (binTreeMinHeight left) (binTreeMinHeight right)