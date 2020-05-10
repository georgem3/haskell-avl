module AVLTree where
data Tree a = EmptyTree | Node (a,Int) (Tree a) (Tree a) deriving (Eq, Read)

-- naive insertion without balancing 
insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node (x,0) EmptyTree EmptyTree
insert x (Node (v,b) l r)
    | x < v = Node (v,b) (insert x l) r
    | x > v = Node (v,b) l (insert x r)
    | otherwise = Node (x,b) l r

-- returns the height of a giving tree
height :: Tree a -> Int
height EmptyTree = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- checks if a tree is balanced (i.e. the balance factor is in [-1,1])
isBalanced :: (Ord a) => Tree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node v l r) | not (isBalanced l) = False
                        | not (isBalanced r) = False
                        | abs ((height l) - (height r)) > 1 = False
                        | otherwise = True

-- helper to quickly build a tree from an integer list
fromList :: [Integer] -> Tree Integer
fromList = foldr insert EmptyTree . reverse

-- helper for printing out a tree
instance (Show a) => Show (Tree a) where
    show t = "root \t -> \t leaf \n" ++ print t 0 where
        print :: (Show a) => (Tree a) -> Int -> String
        print EmptyTree _ = ""
        print (Node (v,b) l r) space = right ++ "\n" ++ (emptyLoop v space) ++ left where
            right = print r (space+6)
            left = print l (space+6)
            emptyLoop v 0 = show v
            emptyLoop v s = " " ++ emptyLoop v (s-1)

-- test trees
tree1 :: Tree Int
tree1 = Node (3,0) (Node (1,0) EmptyTree EmptyTree) (Node (5,0) ((Node (4,0) EmptyTree EmptyTree)) ((Node (8,0) ((Node (7,0) EmptyTree EmptyTree)) ((Node (15,0) EmptyTree EmptyTree))))) 
tree2 :: Tree Int
tree2 = Node (3,0) (Node (1,0) EmptyTree (Node (2,0) EmptyTree EmptyTree)) (Node (5,0) ((Node (4,0) EmptyTree EmptyTree)) ((Node (8,0) ((Node (7,0) EmptyTree EmptyTree)) ((Node (15,0) EmptyTree EmptyTree))))) 
tree3 :: Tree Int
tree3 = Node (3,0) (Node (1,0) EmptyTree EmptyTree) (Node (5,0) ((Node (4,0) EmptyTree EmptyTree)) ((Node (8,0) EmptyTree EmptyTree))) 