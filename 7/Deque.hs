module Deque where

type Deque a = [a] 

makeDequeFromList :: [a] -> Deque a
makeDequeFromList xs = xs

pushFront :: a -> Deque a -> Deque a
pushFront a xs = (a:xs)

pushEnd :: a -> Deque a -> Deque a
pushEnd a xs = (foldr (:) [a] xs)

peekFront :: Deque a -> Maybe a
peekFront xs
    | null xs = Nothing
    | otherwise = (Just (head xs))

peekEnd :: Deque a -> Maybe a
peekEnd xs
    | null xs = Nothing
    | otherwise = (Just (last xs))

popFront :: Deque a -> Deque a
popFront xs
    | null xs = []
    | otherwise = tail xs

popEnd :: Deque a -> Deque a
popEnd xs
    | null xs = []
    | otherwise = init xs