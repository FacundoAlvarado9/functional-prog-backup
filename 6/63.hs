import MyStack
import MyQueue

--- Tests Stack

testMakeStack = makeStackFromList []
testMakeStack2 = makeStackFromList [4,5,6]

testPush = push 3 (makeStackFromList [4,5,6])

testPop = pop (makeStackFromList [3,4,5,6])

testTop = top (makeStackFromList [])
testTop2 = top (makeStackFromList [1,2,3])

--- Tests Queue

testMakeQueue = makeQueueFromList []
testMakeQueue2 = makeQueueFromList [4,5,6]

testEnqueue = enqueue 7 (makeQueueFromList [4,5,6])

testDequeue = dequeue (makeQueueFromList [3,4,5,6])
testDequeue2 = dequeue (makeQueueFromList [])

testFront = front (makeQueueFromList [])
testFront = front (makeQueueFromList [1,2,3])