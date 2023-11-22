-- initial definitions
nat = [0 ..]

f :: Int -> Int -> Int
f x a = (x*x) + x + a -- f(x,a) = x^2 + x + a

---------------
f_a = map (flip f) nat -- stream of functions (Int -> Int), [f_0, f_1, ..., f_n]

f_n :: Int -> [Int] -- apply f_n to all naturals, [f_n(0), f_n(1), ..., f_n(n')]
f_n n = map (f_a!!n) nat -- e.g. f_n 0 = f_0 = x^2 + x + 0

testFn = take 10 (f_n 0) -- [0,2,6,12,20,30,42,56,72,90]

f_all = map f_n nat -- [[f_0(0), f_0(1), ..., f_0(n)], [f_1(0), f_1(1), ..., f_1(n)], ...]
-- f_all should be of type ([Int -> [Int]])

-------

ttake :: Int -> Int -> [[a]] -> [a]
ttake a b xs = take a (xs!!b)

testTtake1 = ttake 10 0 f_all
testTtake2 = ttake 10 1 f_all
testTtake3 = ttake 10 10 f_all