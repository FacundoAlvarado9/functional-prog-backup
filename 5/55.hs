import Data.Bifoldable
import Data.Bifunctor

data Either_ a b = Left_ a | Right_ b deriving (Show)
data Pair a b = Pair a b deriving Show

instance Bifunctor Either_ where
    bimap f g (Left_ x) = Left_ (f x)
    bimap f g (Right_ y) = Right_ (g y)

testBifuncEitherL = first (++" morning") (Left_ "Good")
testBifuncEitherR = second (*3) (Right_ 9)

instance Bifunctor Pair where
    bimap f g (Pair x y) = Pair (f x) (g y)

testBifuncPair = bimap (++"wurst") (+1) (Pair "Brat" 5)

-----
--bifoldable

instance Bifoldable Pair where
    bifoldr f g d (Pair x y) = f x (g y d) -- [f :: (a -> c -> c)] [g :: (b -> c -> c)] [Pair a b]

instance Bifoldable Either_ where -- [Left_ a | Right_ b]
    bifoldr f g d (Left_ x) = (f x d)
    bifoldr f g d (Right_ y) = (g y d)

-----
--Suggested Tests
sugTest1 = bimap (+2) (show) (Pair 1 10) -- Pair 3 "10"
sugTest2 = bimap (+1) (+2) (Left_ 1) -- Left_ 2
sugTest3 = bimap (+1) (+2) (Right_ 1) -- Right_ 3

sugTest4 = bifoldr (+) (*) 3 (Pair 2 10) -- 32
sugTest5 = bifoldr (+) (*) 3 (Left_ 1) -- 4
sugTest6 = bifoldr (+) (*) 3 (Right_ 1) -- 3