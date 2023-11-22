data Set a = Set [a]

---------Show-------

instance (Show a) => Show (Set a) where
    show (Set a) = "{" ++ showListContent a ++ "}"

showListContent :: Show a => [a] -> String
showListContent [] = "" -- so you can show an empty set
showListContent [x] = show x -- In order not to show ',' at the end
showListContent (x:xs) = show x ++ ", " ++ showListContent xs

createSet :: Eq a => [a] -> Set a
createSet [] = Set []
createSet (x:xs) = createSet' (x:xs) []

createSet' :: Eq a => [a] -> [a] -> Set a
createSet' [] acc = Set acc
createSet' (x:xs) acc
    | isOnSecondList x acc = createSet' xs acc
    | otherwise = createSet' xs (acc++[x])

testShow1 = show(createSet[1,2,3,4])
testShow2 = show(createSet[ (createSet [1,2]) , (createSet [3,4]) ])

union :: Eq a => Set a -> Set a -> Set a
union (Set x) (Set y) = createSet(x++y)

------Intersection-------

intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set x) (Set y) = createSet(listIntersection x y)
testInt = intersection (createSet [1,3,4,7,2]) (createSet [2,4,1,7])

isOnSecondList :: Eq a => a -> [a] -> Bool
isOnSecondList x (y:ys)
    | x == y = True
    | x /= y = isOnSecondList x ys
    | otherwise = False
isOnSecondList x [] = False

listIntersection :: Eq a => [a] -> [a] -> [a]
listIntersection [] _ = []
listIntersection _ [] = []
listIntersection (x:xs) (y:ys)
    | isOnSecondList x (y:ys) = [x] ++ (listIntersection xs (y:ys))
    | otherwise = listIntersection xs (y:ys)

-----setMinus-----
setMinus :: Eq a => Set a -> Set a -> Set a
setMinus (Set x) (Set y) = createSet(listAminusB x y)


listAminusB :: Eq a => [a] -> [a] -> [a]
listAminusB [] _ = []
listAminusB _ [] = []
listAminusB (x:xs) (y:ys)
    | isOnSecondList x (y:ys) = (listAminusB xs (y:ys))
    | otherwise = [x] ++ (listAminusB xs (y:ys))

testSetMinus = setMinus (createSet [1,2,3]) (createSet [3,5]) -- {1,2}
testSetMinus1 = setMinus (createSet [1,2,3]) (createSet [4,5]) -- {1,2,3}

-------Comparison
instance (Eq a) => Eq (Set a) where
    (Set a) == (Set b) = a == b -- equality of lists cares about order (we should not)

instance (Ord a) => Ord (Set a) where    
    (<=) (Set a) (Set b) = listContains a b

testCompare = createSet([1,2]) < createSet([1,2,3,4])
testCompareFalse = createSet([1,2,3,4]) < createSet([1,2,4])

listContains :: Eq a => [a] -> [a] -> Bool -- x C y
listContains [] _ = True
--listContains _ [] = False
listContains (x:xs) (y:ys)
    | isOnSecondList x (y:ys) = listContains xs (y:ys)
    | otherwise = False