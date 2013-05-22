
import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

toStringT :: Tree Char -> String
toStringT node = toStringT' "" node
    where toStringT' str EmptyTree = 'e':str
          toStringT' str (Node r leftsub rightsub) = 'n':r:(toStringT' (toStringT' str rightsub) leftsub)

fromStringT :: String -> Tree Char
fromStringT str = if snd parse == "" then fst parse else error "i'm error cure me"
    where parse = parse' str
            where parse' ('e':xs) = (EmptyTree, xs)
                  parse' ('n':x:xs) = let (leftsub, second) = parse' xs in
                                         let (rightsub, rest) = parse' second in
                                             (Node x leftsub rightsub, rest)
                  parse' _ = error "i'm error cure me"