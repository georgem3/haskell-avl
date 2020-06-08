module AVLTree where
data Tree a = EmptyTree 
            | Node a (Tree a) (Tree a) 
            -- corresponds to (nodeContent) leftTree rightTree
            deriving (Eq, Read)

-- returns a balanced tree after inserting a given element
insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node x EmptyTree EmptyTree
insert x (Node v l r)
    | x < v = rebalance $ (Node v (insert x l) r)
    | otherwise = rebalance $ (Node v l (insert x r))

-- deletes a given element if found, without rebalancing the tree
delete :: (Ord a) => a -> Tree a -> Tree a 
delete _ EmptyTree = EmptyTree
delete x t@(Node v l r)
    | x < v = (Node v (delete x l) r)
    | x > v = (Node v l (delete x r))
    | x == v && l == EmptyTree = r
    | x == v && r == EmptyTree = l
    | otherwise = (Node succ (delete succ l) r) 
    where
        succ = inOrderSucc t

inOrderSucc :: Tree a -> a 
inOrderSucc (Node v EmptyTree _)  = v
inOrderSucc (Node _ l _)  = inOrderSucc l

-- rotates a tree around its root to the right
rotateR :: Tree a -> Tree a
rotateR EmptyTree = EmptyTree
rotateR (Node x (Node y sl sr) r) = Node y sl (Node x sr r)

-- rotates a tree around its root to the left
rotateL :: Tree a -> Tree a
rotateL EmptyTree = EmptyTree
rotateL (Node x l (Node y sl sr)) = Node y (Node x l sl) sr

-- rebalances a tree
rebalance :: Tree a -> Tree a
rebalance EmptyTree = EmptyTree
rebalance t@(Node v l r)
    -- op didnt require rebalancing, balance factor is still in range
    | abs (diff t) < 2        = t
    -- new node was created left.left of root
    | diff t == 2 && dl /= -1 = rotateR t
    -- new node was created left.right of root
    | diff t == 2 && dl == -1 = rotateR (Node v (rotateL l) r)
    -- new node was created right.right of root
    | diff t == -2 && dr /= 1 = rotateL t
    -- new node was created right.left of root
    | diff t == -2 && dr == 1 = rotateL (Node v l (rotateR r))
    where
        dl = diff l
        dr = diff r

-- helper to return the height value of a given tree
height :: Tree a -> Int
height EmptyTree = 0
height (Node _ l r) = 1 + max (height l) (height r)

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
        print (Node v l r) space = right ++ "\n" ++ (emptyLoop v space) ++ left where
            right = print r (space+6)
            left = print l (space+6)
            emptyLoop v 0 = show v
            emptyLoop v s = " " ++ emptyLoop v (s-1)

-- test trees
tree1 :: Tree Int
tree1 = Node 3 (Node 1 EmptyTree EmptyTree) (Node 5 ((Node 4 EmptyTree EmptyTree)) ((Node 8 ((Node 7 EmptyTree EmptyTree)) ((Node 15 EmptyTree EmptyTree))))) 
tree2 :: Tree Int
tree2 = Node 3 (Node 1 EmptyTree (Node 2 EmptyTree EmptyTree)) (Node 5 ((Node 4 EmptyTree EmptyTree)) ((Node 8 ((Node 7 EmptyTree EmptyTree)) ((Node 15 EmptyTree EmptyTree))))) 
tree3 :: Tree Int
tree3 = Node 3 (Node 1 EmptyTree EmptyTree) (Node 5 ((Node 4 EmptyTree EmptyTree)) ((Node 8 EmptyTree EmptyTree))) 