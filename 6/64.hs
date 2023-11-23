import MVector

test = vectorFromList [1,2,3]

testSkalarMult = skalarMult 2 (vectorFromList [1,2,3])
testSkalarMult2 = skalarMult 3 (vectorFromList [])

testVecAdd_Fail = vecAdd (vectorFromList [1,2]) (vectorFromList [1,2,3])
testVecAdd = vecAdd (vectorFromList [1,2,3]) (vectorFromList [1,2,3])
testVecAdd2 = vecAdd (vectorFromList []) (vectorFromList [])

testVecProd_Fail = vecProd (vectorFromList [1,2]) (vectorFromList [1,2,3])
testVecProd = vecProd (vectorFromList [1,2,3]) (vectorFromList [4,5,6]) -- 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32

testCrossProd = crossProd (vectorFromList [2,0,1]) (vectorFromList [1,-1,3])
testCrossProd_Fail = crossProd (vectorFromList [2,0,1]) (vectorFromList [1,-1])