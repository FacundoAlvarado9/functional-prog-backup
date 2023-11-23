import SortedList

testCreate = createSortedList ([]::[Int])

testCreate2 = (createSortedList [9, (-1), 6,5,4,3,2,19,(-23)])

testHead = head' (createSortedList [9,8,7,6,5,4,3,2,(-25)])
testTail = tail' (createSortedList [9,8,7,6,5,4,3,2,(-25)])

testNull = null' (createSortedList ([]::[Int]))
testNull2 = null' (createSortedList [9,8,7,6,5,4,3,2,(-25)])

testPrepend = 3#(createSortedList [1,2,4,5])
testPrepend2 = 2#(createSortedList [1,2,4,5])
testPrepend3 = 2#(createSortedList [])