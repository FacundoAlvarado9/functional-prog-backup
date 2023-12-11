module SortedList (SortedList, (#), insert', createSortedList, head', tail', null', minimum', empty', insertOrdered, maximum', last', length') where

data SortedList a = SortedList [a] deriving (Show)

empty' :: SortedList a
empty' = SortedList []

minimum' :: Ord a => SortedList a -> a
minimum' (SortedList xs) = minimum xs

maximum' :: Ord a => SortedList a -> a
maximum' (SortedList xs) = maximum xs

insertOrdered :: Ord a => a -> [a] -> [a]
insertOrdered x [] = [x]
insertOrdered x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(insertOrdered x ys)

isSorted :: Ord a => SortedList a -> Bool
isSorted (SortedList []) = True
isSorted (SortedList [x]) = True
isSorted (SortedList (x:y:xs)) = (x<=y) && isSorted(SortedList (y:xs))

insert' :: Ord a => a -> SortedList a -> SortedList a
insert' x (SortedList xs) = SortedList (insertOrdered x xs)

(#) :: Ord a => a -> SortedList a -> SortedList a
x # (SortedList xs) = SortedList (insertOrdered x xs)

createSortedList :: Ord a => [a] -> SortedList a
createSortedList [] = (SortedList [])
createSortedList (x:xs) = x#(createSortedList xs)

head' :: SortedList a -> a
head' (SortedList (x:xs)) = x

last' :: SortedList a -> a
last' (SortedList xs) = last xs

tail' :: SortedList a -> SortedList a
tail' (SortedList (x:xs)) = (SortedList xs)

length' :: SortedList a -> Int
length' (SortedList xs) = (length xs)

null' :: SortedList a -> Bool
null' (SortedList xs) = (null xs)