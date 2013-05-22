data BST = MakeNode BST String BST
          |  Empty
                                                      
listToBST :: [String] -> BST
listToBST = foldr add Empty

bstToList :: BST -> [String]
bstToList = flip go []
  where
    -- Uses a difference list for efficient appends
    go :: BST -> [String] -> [String]
    go Empty = id
    go (MakeNode l p r) = go l . (p:) . go r