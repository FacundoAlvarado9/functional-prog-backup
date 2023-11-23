module MyQueue where

import Deque

data Queue a = Queue (Deque a) deriving (Show)

-- Since we are using the underlying data type (Deque a), the complexity
-- of the operations here (Queue) is the complexity of the underlying operations

makeQueueFromList :: [a] -> Queue a ---- O(1)
makeQueueFromList xs = Queue (makeDequeFromList xs)

enqueue :: a -> Queue a -> Queue a ---- O(n)
enqueue x (Queue dq) = Queue (pushEnd x dq)

dequeue :: Queue a -> Queue a ---- O(1)
dequeue (Queue dq) = Queue (popEnd dq)

front :: Queue a -> Maybe a ---- O(1)
front (Queue dq) = peekFront dq