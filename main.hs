module AVLTree where
import Debug.Trace
data Tree a = EmptyTree 
            | Node (a,Int) (Tree a) (Tree a) 
            -- corresponds to (nodeContent, nodeHeight) leftTree rightTree
            deriving (Eq, Read)

-- returns a balanced tree after inserting a given element
insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node (x,1) EmptyTree EmptyTree
insert x t@(Node (v,_) l r)
    | x < v = rebalance $ (Node (v,hl) (insert x l) r)
    | otherwise = rebalance $ (Node (v,hr) l (insert x r)) where
        hl = (1 + max (height (insert x l)) (height r))
        hr = (1 + max (height l) (height (insert x r))) 

-- deletes a given element if found
delete :: (Ord a) => a -> Tree a -> Tree a 
delete _ EmptyTree = EmptyTree
delete x t@(Node (v,h) l r)
    | x < v = rebalance (Node (v,hl) (delete x l) r)
    | x > v = rebalance (Node (v,hr) l (delete x r))
    | x == v && l == EmptyTree = r
    | x == v && r == EmptyTree = l
    | otherwise = (Node (succ,1) (delete succ l) r) 
    where
        hl = (1 + max (height (delete x l)) (height r))
        hr = (1 + max (height l) (height (delete x r))) 
        succ = inOrderSucc $ left t
        -- inOrderSucc is technically a partial function, but recursion handles EmptyTree beforehand
        inOrderSucc :: (Ord a) => Tree a -> a
        inOrderSucc (Node (v,_) _ EmptyTree) = v
        inOrderSucc (Node _ _ r)             = inOrderSucc r

-- rebalances a tree
rebalance :: Tree a -> Tree a
rebalance EmptyTree = EmptyTree
rebalance t@(Node (v,h) l r)
    -- op didnt require rebalancing, balance factor is still in range
    | abs (diff t) < 2        = t
    -- new node was created left.left of root
    | diff t == 2 && dl /= -1 = rotateR t
    -- new node was created left.right of root
    | diff t == 2 && dl == -1 = rotateR (Node (v,h) (rotateL l) r)
    -- new node was created right.right of root
    | diff t == -2 && dr /= 1 = rotateL t
    -- new node was created right.left of root
    | diff t == -2 && dr == 1 = rotateL (Node (v,h) l (rotateR r))
    -- tree wasnt an AVL Tree to begin with
    | otherwise = t
    where
        dl = diff l
        dr = diff r

        -- returns the height difference of a tree's children
        diff :: Tree a -> Int
        diff EmptyTree = 0
        diff (Node _ l r) = (height l) - (height r)

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

-- Not part of the data structure implementation
-- helper to return the height of a given tree's root
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

-- helper to quickly build a tree from an integer list
fromList :: [Int] -> Tree Int
fromList = foldr insert EmptyTree

-- helper for printing out a tree
instance (Show a) => Show (Tree a) where
    show t = "root \t -> \t leaf \n" ++ print t 0 where
        print :: (Show a) => (Tree a) -> Int -> String
        print EmptyTree _ = ""
        print (Node (v,b) l r) space = right ++ "\n" ++ (emptyLoop (v,b) space) ++ left where
            right = print r (space+6)
            left = print l (space+6)
            emptyLoop (v,b) 0 = show v ++ "," ++ show b
            emptyLoop (v,b) s = " " ++ emptyLoop (v,b) (s-1)

-- test trees
tree1 :: Tree Int
tree1 = Node (3,4) (Node (1,1) EmptyTree EmptyTree) (Node (5,3) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,2) ((Node (7,1) EmptyTree EmptyTree)) ((Node (15,1) EmptyTree EmptyTree))))) 
tree2 :: Tree Int
tree2 = Node (3,4) (Node (1,2) EmptyTree (Node (2,1) EmptyTree EmptyTree)) (Node (5,3) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,2) ((Node (7,1) EmptyTree EmptyTree)) ((Node (15,1) EmptyTree EmptyTree)))))
tree3 :: Tree Int
tree3 = Node (3,3) (Node (1,1) EmptyTree EmptyTree) (Node (5,2) ((Node (4,1) EmptyTree EmptyTree)) ((Node (8,1) EmptyTree EmptyTree)))