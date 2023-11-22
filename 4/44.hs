dfold :: (b -> c -> c) -> c -> (a -> b -> b) -> b -> [[a]] -> c
dfold f i g j xs = foldr f i (map (foldr g j) xs)

testDFold = dfold (+) 0 (*) 1 [[1,2,3], [1,4,1], [2,2,2,1]]

--------------------
nfilter' :: [(a -> Bool)] -> [a] -> [a]
nfilter' [] ys = ys -- (as help for reasoning)
nfilter' (x:xs) ys = nfilter xs (filter x ys)

nfilter :: [(a -> Bool)] -> [a] -> [a]
nfilter xs ys = foldr filter ys xs