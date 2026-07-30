module SearchTree 
  ( SearchTree
  , empty
  , insert
  , delete
  ) where


#ifdef TEMPLATE
-- |Search tree.
data SearchTree a = Empty
                  | Node (SearchTree a) a (SearchTree a)
  deriving Show


-- |Empty search tree.
empty :: SearchTree a
empty = Empty

-- |Inserts an element into a search tree.
insert :: Ord a => a -> SearchTree a -> SearchTree a
insert x Empty        = Node Empty x Empty
insert x (Node l y r)
  | x < y     = Node (insert x l) y r
  | x > y     = Node l y (insert x r)
  | otherwise = Node l y r 

-- |Extracts the minimum from a search tree.
extractMin :: SearchTree a -> (a, SearchTree a)
extractMin (Node Empty x r) = (x, r)
extractMin (Node l x r)     = (y, Node l' x r)
  where (y, l') = extractMin l

-- |Delete an element from a search tree.
delete :: Ord a => a -> SearchTree a -> SearchTree a
delete _ Empty        = Empty
delete x (Node l y r)
  | x < y     = Node (delete x l) y r
  | x > y     = Node l y (delete x r)
  | otherwise = case r of
                  Empty -> l
                  _     -> Node l m r'
  where (m, r') = extractMin r

rotate :: SearchTree a -> SearchTree a
rotate = error "not implemented"
#else
-- |Search tree.
data SearchTree a = Empty
                  | Node (SearchTree a) a Int (SearchTree a)
  deriving Show


-- |Empty search tree.
empty :: SearchTree a
empty = Empty

-- |Retrieves the height of a node.
height :: SearchTree a -> Int
height Empty          = -1
height (Node _ _ h _) = h

balance :: SearchTree a -> Int
balance Empty = 0
balance (Node l _ _ r) = height r - height l

-- |Restores the AVL property.
avl :: SearchTree a -> SearchTree a
avl = rotate . fixHeight

fixHeight :: SearchTree a -> SearchTree a
fixHeight Empty          = Empty
fixHeight (Node l x h r) = Node l x (max (height l) (height r) + 1) r

-- |Inserts an element into a search tree.
insert :: Ord a => a -> SearchTree a -> SearchTree a
insert x Empty          = Node Empty x 0 Empty
insert x (Node l y h r)
  | x < y     = avl (Node (insert x l) y h r)
  | x > y     = avl (Node l y h (insert x r))
  | otherwise = Node l y h r 

-- |Extracts the minimum from a search tree.
extractMin :: SearchTree a -> (a, SearchTree a)
extractMin (Node Empty x _ r) = (x, r)
extractMin (Node l x h r)     = (y, avl (Node l' x h r))
  where (y, l') = extractMin l

-- |Delete an element from a search tree.
delete :: Ord a => a -> SearchTree a -> SearchTree a
delete _ Empty          = Empty
delete x (Node l y h r)
  | x < y     = avl (Node (delete x l) y h r)
  | x > y     = avl (Node l y h (delete x r))
  | otherwise = case r of
                  Empty -> l
                  _     -> avl (Node l m h r')
  where (m, r') = extractMin r

-- |Rotates a node if the AVL property is violated
rotate :: SearchTree a -> SearchTree a
rotate Empty               = Empty
rotate node@(Node l x h r) =
  case (balance l, balance node, balance r) of
    (_, 2, -1) -> rotateL (Node l x h (rotateR r))
    (_, 2, _)  -> rotateL node
    (_, -2, 1) -> rotateR (Node (rotateL l) x h r)
    (_, -2, _) -> rotateR node
    _          -> node
  where
    rotateL (Node l x _ (Node rl y _ rr)) = fixHeight (Node (fixHeight (Node l x 0 rl)) y 0 rr)
    rotateR (Node (Node ll x _ lr) y _ r) = fixHeight (Node ll x 0 (fixHeight (Node lr y 0 r)))
#endif

