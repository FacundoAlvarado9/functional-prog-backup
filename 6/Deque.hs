module Deque where

data Deque a = Deque [a] deriving (Show)

makeDequeFromList :: [a] -> Deque a
makeDequeFromList xs = (Deque xs)

pushFront :: a -> Deque a -> Deque a
pushFront a (Deque xs) = (Deque (a:xs))

pushEnd :: a -> Deque a -> Deque a
pushEnd a (Deque xs) = Deque (foldr (:) [a] xs)

peekFront :: Deque a -> Maybe a
peekFront (Deque xs)
    | null xs = Nothing
    | otherwise = (Just (head xs))

peekEnd :: Deque a -> Maybe a
peekEnd (Deque xs)
    | null xs = Nothing
    | otherwise = (Just (last xs))

popFront :: Deque a -> Deque a
popFront (Deque xs)
    | null xs = (Deque [])
    | otherwise = (Deque (tail xs))

popEnd :: Deque a -> Deque a
popEnd (Deque xs)
    | null xs = (Deque [])
    | otherwise = (Deque (init xs))