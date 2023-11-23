module SortedList (SortedList, (#), createSortedList, head', tail', null') where

data SortedList a = SortedList [a] deriving (Show)

insertOrdered :: Ord a => a -> [a] -> [a]
insertOrdered x [] = [x]
insertOrdered x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(insertOrdered x ys)

(#) :: Ord a => a -> SortedList a -> SortedList a
x # (SortedList xs) = SortedList (insertOrdered x xs)

createSortedList :: Ord a => [a] -> SortedList a
createSortedList [] = (SortedList [])
createSortedList (x:xs) = x#(createSortedList xs)

head' :: SortedList a -> a
head' (SortedList (x:xs)) = x

tail' :: SortedList a -> SortedList a
tail' (SortedList (x:xs)) = (SortedList xs)

length' :: SortedList a -> Int
length' (SortedList xs) = (length xs)

null' :: SortedList a -> Bool
null' (SortedList xs) = (null xs)