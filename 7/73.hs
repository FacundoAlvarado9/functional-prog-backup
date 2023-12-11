import Debug.Trace

fib :: Integer -> Integer
fib 0 = trace ("fib 0") 0
fib 1 = trace ("fib 1") 1
fib n = trace ("fib " ++ show n ++ " = fib " ++ show (n-1) ++ " + fib " ++ show (n-2)) (fib (n-1) + fib (n-2))

-- From its definition: 
-- trace :: String -> a -> a
-- it looks like a pure function. However, it has the side effect of printing messages. d.h. Not pure

--let's see what a function defined in the same way would do

trace' :: String -> a -> a
trace' s a = a -- There is no way we could display anything close to a message here, unless a is a String in itself
-- therefore, the behavior of trace exceeds the "capabilities" of a normal haskell function.
-- That is, it is not exactly pure.