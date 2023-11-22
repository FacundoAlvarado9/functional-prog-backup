data Triple a b c = Triple a b c deriving (Show)

class Trifunctor p where
    trimap :: (a -> b) -> (c -> d) -> (e -> f) -> p a c e -> p b d f -- p is data type

class Trifoldable p where
    trifoldr :: (a->d->d) -> (b->d->d) -> (c->d->d) -> d -> p a b c -> d

instance Trifunctor Triple where
    trimap f g h (Triple x y z) = Triple (f x) (g y) (h z)

instance Trifoldable Triple where
    trifoldr f g h d (Triple x y z) = (f x (g y (h z d)))

sugTest1 = trimap show (+1) (replicate 3 . show) (Triple 2 3 4)
sugTest2 = trifoldr (+) (*) (^) 2 (Triple 1 2 3)