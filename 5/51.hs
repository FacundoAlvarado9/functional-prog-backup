import Data.List(tails)

nat = [0 ..]

ev = [x | x<-nat, even x]
------

applyHarm :: Int -> Double
applyHarm n = 1/(fromIntegral n)

harmonic = [applyHarm x | x<-(tail nat)]

------
isPalindrome :: Int -> Bool
isPalindrome n = (show n) == (reverse (show n))

palin = [x | x<-nat, isPalindrome x]

------
partsum :: Num n => [n] -> [n]
partsum xs = [x1+x2 | (x1:x2:_) <- tails xs]

-- partsum input = [foldr (+) 0 (take x input) | x <- [0 ..]]