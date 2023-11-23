import Deque

testMakeDeque = makeDequeFromList [1,2,3,4]
testMakeDeque2 = makeDequeFromList []

testPF = pushFront (0) (Deque [1,2,3])
testPF2 = pushFront (0) (Deque [])

testPE = pushEnd (0) (Deque [1,2,3])
testPE2 = pushEnd (0) (Deque [])

testPeek = peekFront (Deque [])
testPeek2 = peekFront (Deque [1,2,3,4])

testPeekEnd = peekEnd (Deque [])
testPeekEnd2 = peekEnd (Deque [1,2,3,4,5])

testPopF = popFront (Deque [0,1,2,3])
testPopF2 = popFront (Deque [])

testPopEnd = popEnd (Deque [])
testPopEnd2 = popEnd (Deque [1,2,3,4])