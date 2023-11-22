data Bool_ a = True_ a | False_ a deriving (Show)-- now :kind Bool_ is (* -> *)

instance Functor Bool_ where
    fmap f (True_ x) = True_ (f x)
    fmap f (False_ x) = False_ (f x)

testBool = fmap (*2) (True_ 5)

--------
data List a = List a (List a) | Empty

instance Functor List where
    fmap f (List x sublist) = List (f x) (fmap f sublist)
    fmap f Empty = Empty

------
data Either_ a b = Left_ a | Right_ b deriving (Show)

instance Functor (Either_ a) where -- constraining one type we get a kind of (* -> *)
    fmap f (Right_  x) = Right_ (f x)
    fmap f (Left_ x) = Left_ x -- following convention of left as possible error message

testEither = fmap (*2) (Right_ 5)
testEitherLeft = fmap (*2) (Left_ "Error!")

------
data Maybe_ a = Nothing_ | Just_ a deriving (Show)

instance Functor Maybe_ where
    fmap f Nothing_ = Nothing_
    fmap f (Just_ x) = Just_ (f x)

testMaybe = fmap (*2) Nothing_
testMaybe2 = fmap (*2) (Just_ 9)

------
data Pair a b = Pair a b deriving (Show)

instance Functor (Pair a) where
    fmap f (Pair a x) = Pair a (f x)

testPair = fmap (*2) (Pair "Pizza" 3)
-------
data LList a = LList [a] (a,a) a deriving (Show)

instance Functor LList where
    fmap f (LList xs (x,y) z) = LList (map f xs) (f x, f y) (f z)

testLList = fmap (*2) (LList [3,5,7] (9,11) 13)