module MapWeighted(
    Map
  , empty
  , lookup
  , put
  , fold
  ) where

-- Weighted Trees in Haskell

import Prelude hiding(lookup)

-- The weight parameter type
type Weight = Int

-- The weight parameter, says when a tree has become unbalanced.
weight :: Weight
weight = 3

-- The data type
data Tree a = Null 
            | Node Weight (Tree a) a (Tree a)

-- Returns the size of the tree
size :: Tree a-> Weight
size Null           = 0
size (Node h _ _ _) = h  

-- Constructs a node, maintains size.
-- Pre: arguments are balanced, and size is set correctly
node :: Tree a-> a-> Tree a-> Tree a
node l n r = Node h l n r where
               h = 1+ size l+ size r

-- One single rotation to the left.
rotl :: Tree a-> Tree a
rotl (Node _ xt y (Node _ yt x zt)) = 
  node (node xt y yt) x zt

-- One single rotation to the right.
rotr :: Tree a-> Tree a
rotr (Node _ (Node _ ut y vt) x rt) = 
  node ut y (node vt x rt)

-- The bias compares the sizes of the left and right subtrees.
bias :: Tree a-> Ordering
bias Null = EQ
bias (Node _ lt _ rt) = compare (size lt) (size rt)

-- Main function: construct a weighted tree.
-- Four main cases: 
-- a. Tree small enough 
-- b. Imbalanced to the right 
-- c. Imbalanced to the left
-- d. Balanced
-- Cases b and c require rotations (double/single, depending on bias)
mkNode :: Tree a-> a-> Tree a-> Tree a
mkNode lt x rt 
  | ls+ rs < 2  = node lt x rt
  | weight* ls < rs  = 
      if bias rt == LT then rotl (node lt x rt)
      else rotl (node lt x (rotr rt))
  | ls > weight* rs =
      if bias lt == GT then rotr (node lt x rt)
      else rotr (node (rotl lt) x rt)
  | otherwise   = node lt x rt where 
    ls = size lt; rs= size rt

-- | Maps built using trees labelled with (key, value) pairs.
type Map a b = Tree (a, b)

-- | The empty tree
empty  :: Map a b
empty  = Null

lookup :: Ord a=> a-> Map a b-> Maybe b
lookup k Null = Nothing
lookup k (Node _ lt (kv, kn) rt) 
  | k == kv  = Just kn
  | k <  kv  = lookup k lt
  | k >  kv  = lookup k rt

put :: Ord a=> a-> Maybe b-> Map a b-> Map a b
put k Nothing Null    = Null
put k (Just val) Null = node Null (k, val) Null
put k v (Node n l a@(kn, _) r) 
  | k < kn  = mkNode (put k v l) a r
  | k == kn = case v of 
       Nothing  -> join l r
       Just val -> Node n l (k, val) r
  | k > kn  = mkNode l a (put k v r)

-- Joins two trees
-- Preconditions: 
--   |size l- size r| <= 1
--   for all node values n in l und m in r, n < m.
join ::  Tree a-> Tree a-> Tree a
join xt Null = xt
join lt rt   = mkNode lt u nu where
  (u, nu) = splitTree rt
  splitTree :: Tree a-> (a, Tree a)
  splitTree (Node h Null a t) = (a, t)
  splitTree (Node h lt a rt) =
    (u, mkNode nu a rt) where
       (u, nu) = splitTree lt

fold :: (a-> b-> b)-> b-> Tree a-> b    
fold f e Null = e
fold f e (Node _ l a r) =
   fold f (f a (fold f e r)) l

toList :: Tree a-> [a]
toList = fold (:) []

-- Slightly fancy Show instance
instance Show a=> Show (Tree a) where
  show t = shw 0 t where
    shw _ Null = []
    shw n (Node _ l a r) = (shw (n+1) l)
                              ++ spc n ++ show a  ++"\n" 
                              ++ (shw (n+1) r)
    spc n = concat (replicate n "   ")

-- Equality on weighted trees as maps is extensional
-- (same nodes, but structure irrelevant)
instance Eq a=> Eq (Tree a) where
  t1 == t2 = toList t1 == toList t2

-- Test tree
t :: [Int]-> Map Int String
t = foldl (\t n -> put n (Just $ show n) t) empty

test :: [Int]-> [Map Int String]
test = scanl (\t n -> put n (Just $ show n) t) empty

t1 = t [4,5,7,2,1,3,6,8,9]
t2 = t [3,1,4,3]
