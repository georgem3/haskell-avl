module AVLTree where
data Tree a = EmptyTree 
            | Node (a,Int) (Tree a) (Tree a) 
            -- corresponds to (nodeContent, height) leftTree rightTree
            deriving (Eq, Read)

-- naive insertion without balancing 
insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node (x,0) EmptyTree EmptyTree
insert x (Node (v,b) l r)
    | x < v = (Node (v,b+1) (insert x l) r)
    | otherwise = (Node (v,b+1) l (insert x r))

-- rotates a tree around its root to the right
rotateR :: Tree a -> Tree a
rotateR EmptyTree = EmptyTree
rotateR (Node (x,h) (Node (y,k) sl sr) r) = Node (y,k') sl (Node (x,h') sr r) where
    h' = 1 + max (height sr) (height r)
    k'  = 1 + max (height sl) h'

-- rotates a tree around its root to the left
rotateL :: Tree a -> Tree a
rotateL EmptyTree = EmptyTree
rotateL (Node (x,h) l (Node (y,k) sl sr)) = Node (y,k') (Node (x,h') l sl) sr where
    h' = 1 + max (height l) (height sl)
    k' = 1 + max (height sr) h'

-- rebalances a tree
rebalance :: Tree a -> Tree a
rebalance EmptyTree = EmptyTree
rebalance t@(Node (v,h) l r)
    -- op didnt require rebalancing, balance factor is still in range
    | abs (diff t) < 2        = t
    -- new node was created left.left of root
    | diff t == 2 && dl /= -1 = rotateR t
    -- new node was created left.right of root
    | diff t == 2 && dl == -1 = rotateR (Node (v,h-1) (rotateL l) r)
    -- new node was created right.right of root
    | diff t == -2 && dr /= 1 = rotateL t
    -- new node was created right.left of root
    | diff t == -2 && dr == 1 = rotateL (Node (v,h-1) l (rotateR r))
    where
        dl = diff l
        dr = diff r

-- helper to return the height value of a given tree
height :: Tree a -> Int
height EmptyTree = 0
height (Node (_,h) l r) = h

-- helper to return the left child of a tree
left :: Tree a -> Tree a
left EmptyTree = EmptyTree
left (Node _ l _) = l

-- helper to return the right child of a tree
right :: Tree a -> Tree a
right EmptyTree = EmptyTree
right (Node _ _ r) = r

-- returns the height difference of a tree's children
diff :: Tree a -> Int
diff EmptyTree = 0
diff (Node v l r) = (height l) - (height r)

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
tree1 = Node (3,4) (Node (1,1) EmptyTree EmptyTree) (Node (5,3) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,2) ((Node (7,1) EmptyTree EmptyTree)) ((Node (15,1) EmptyTree EmptyTree))))) 
tree2 :: Tree Int
tree2 = Node (3,4) (Node (1,2) EmptyTree (Node (2,1) EmptyTree EmptyTree)) (Node (5,3) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,2) ((Node (7,1) EmptyTree EmptyTree)) ((Node (15,1) EmptyTree EmptyTree))))) 
tree3 :: Tree Int
tree3 = Node (3,3) (Node (1,1) EmptyTree EmptyTree) (Node (5,2) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,1) EmptyTree EmptyTree))) 