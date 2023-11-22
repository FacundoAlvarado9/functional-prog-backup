nat = [0 ..]

ev = filter even nat

------
nat1 = [1 ..] -- Valid? Since division by 0 is not possible.

applyHarm :: Double -> Double
applyHarm n = 1/n

harmonic = map applyHarm nat1

------
nat' = [1 ..] --could have used tail on the nat to drop the 0

aux2 :: [Int] -> Int
aux2 xs = foldr (+) 0 xs

getTriangle :: Int -> Int
getTriangle n = aux2 (take n nat')

triangle = map getTriangle nat'

------
isPalindrome :: Int -> Bool
isPalindrome n = (show n) == (reverse (show n))

palin = filter isPalindrome nat