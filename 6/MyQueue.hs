module MyQueue where

import Deque

data Queue a = Queue (Deque a) deriving (Show)

-- Since we are using the underlying data type (Deque a), the complexity
-- of the operations here (Queue) is the complexity of the underlying operations

makeQueueFromList :: [a] -> Queue a ---- O(1)
makeQueueFromList xs = Queue (makeDequeFromList xs)

enqueue :: a -> Queue a -> Queue a ---- O(n)
enqueue x (Queue dq) = Queue (pushEnd x dq)
-- --> Could be implemented more efficiently by using an underlying data type that appends to the end
-- in constant time. For example, Data.Sequence (https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html)


dequeue :: Queue a -> Queue a ---- O(1)
dequeue (Queue dq) = Queue (popEnd dq)

front :: Queue a -> Maybe a ---- O(1)
front (Queue dq) = peekFront dq