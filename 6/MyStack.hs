module MyStack where

import Deque

data Stack a = Stack (Deque a) deriving Show

-- Since we are using the underlying data type (Deque a), the complexity
-- of the operations here (Stack) is the complexity of the underlying operations

makeStackFromList :: [a] -> Stack a ---- O(1)
makeStackFromList xs = Stack (makeDequeFromList xs)

push :: a -> Stack a -> Stack a ---- O(1)
push x (Stack dq) = Stack (pushFront x dq)

pop :: Stack a -> Stack a ----- O(1)
pop (Stack dq) = Stack (popFront dq)

top :: Stack a -> Maybe a ---- O(1)
top (Stack dq) = peekFront dq