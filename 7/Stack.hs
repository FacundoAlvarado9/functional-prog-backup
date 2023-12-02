module Stack (St, empty, push, pop, top) where

data St a = St [a]  deriving (Show, Eq)

empty :: St a
empty =  St []

push :: a-> St a-> St a
push a (St s) = St (a:s)

top  :: St a-> a
top  (St [])  = error "St: top on empty stack"
top  (St s)   = head s

pop  :: St a-> St a
pop  (St [])  = error "St: pop on empty stack"
pop  (St s)   = St (tail s)
