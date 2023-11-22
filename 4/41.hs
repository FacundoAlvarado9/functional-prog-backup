product' :: [Int] -> Int
product' xs = foldl (*) 1 xs

testProd = product' [1 .. 5]
testProd2 = product' []
testProd3 = product' [2,4,-1]

----------------
allOdd :: [Int] -> Bool
allOdd xs = foldr (&&) True (map odd xs) -- use of foldr since is non-strict

testOdd = allOdd[5,7] -- True
testOdd2 = allOdd[4] -- False

-----------------
mathOperation :: Integer -> Integer
mathOperation x = ((x*x) + 3*x + 5) -- x^2 + 3x + 5

obtainPair :: Integer -> (Integer, Integer)
obtainPair n = (n, mathOperation n)

kinda :: [(Integer, Integer)]
kinda = map obtainPair [0 .. 150]

---------
hasKey :: String -> (String, Int) -> Bool
hasKey k (s,n) = (k == s)

getSecondElem :: (a,b) -> b
getSecondElem (x,y) = y

getByKey :: [(String, Int)] -> String -> [Int]
getByKey xs k = map getSecondElem(filter (hasKey k) xs)

testGBK = getByKey [("a", 2), ("b", 3), ("a", 6), ("c", 4)] "b"
testGBKNotThere = getByKey [("a", 2), ("b", 3), ("a", 6), ("c", 4)] "d"