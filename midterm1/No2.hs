data BinTree a = EmptyBinTree
                  | BinTreeNode a (BinTree a) (BinTree a)
		  deriving (Eq, Ord, Read, Show)

binTreeFromList :: (Ord) a => [a] -> BinTree a
binTreeFromList = foldl insert EmptyBinTree
    where insert EmptyBinTree                    x             = BinTreeNode x EmptyBinTree EmptyBinTree
          insert node@(BinTreeNode root left right) x | x < root     = BinTreeNode root (insert left x) right
                                                      | x > root     = BinTreeNode root left (insert right x)
                                                      | otherwise = node
                                                      
                                                      
                                                      
treeLess :: BinTree a -> [a]
treeLess EmptyBinTree = []
treeLess (BinTreeNode root left right) = treeLess' (BinTreeNode root left right) []
  where treeLess' (BinTreeNode root left right) m  
						  | left == EmptyBinTree && right == EmptyBinTree = (root:m)
						  | left /= EmptyBinTree = 
  --where rest = ((treeLess left):(treeLess right))