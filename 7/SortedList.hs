module SortedList (SortedList, (#), createSortedList, head', tail', null', isSorted) where

import Data.List

type SortedList a = [a]

insertOrdered :: Ord a => a -> [a] -> [a]
insertOrdered x [] = [x]
insertOrdered x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(insertOrdered x ys)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = (x<=y) && isSorted(y:xs)

(#) :: Ord a => a -> SortedList a -> SortedList a
x # xs = (insertOrdered x xs)

createSortedList :: Ord a => [a] -> SortedList a
createSortedList [] = []
createSortedList (x:xs) = x#(createSortedList xs)

head' :: Ord a => SortedList a -> a
head' xs
    | isSorted xs = head xs
    | otherwise = head (sort xs)

tail' :: SortedList a -> SortedList a
tail' (x:xs) = xs

length' :: SortedList a -> Int
length' xs = (length xs)

null' :: SortedList a -> Bool
null' xs = (null xs)