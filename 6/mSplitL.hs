module MSplitL where

splitL :: Int -> [a] -> [[a]]
splitL n [] = [[], []]
splitL n (x:xs)
    | n < 0 = [[], x:xs]
    | otherwise = [x:a, b]
    where [a,b] = splitL (n-1) xs

testBaseCase = splitL (-1) "Apfelsaft"
test1 = splitL 0 "Apfelsaft" -- ["A", "pfelsaft"]
test2 = splitL 5 "Apfelsaft" -- ["Apfel", "saft"]
testConcat = concat(splitL 5 "Apfelsaft")
testLastChar = splitL 2 "foo" -- ["foo", ""]
testOutsideRange = splitL 3 "foo" -- ["foo", ""]

mSplitL :: [Int] -> [a] -> [[a]]
mSplitL [] l = [l]
mSplitL (y:ys) [] = (mSplitL ys [])++[[]] -- in order to comply with length(mSplitL ints lst) = length(inst) + 1
mSplitL (y:ys) (x:xs) = (splitL y a)++(ts) where (a:ts) = mSplitL ys (x:xs)

mTest = mSplitL [1, 3, 5] "foobar"
mTest2 = mSplitL [1, 2] "foobar"
mTestConcat = concat(mSplitL [1, 3, 5] "foobar")
mTestLength = (length(mSplitL [1, 3, 5] "foobar") == length([1,3,5]) + 1)