module MVector(Vector, vectorFromList, skalarMult, vecAdd, vecProd, crossProd) where

data Vector a = Vector [a] deriving (Show)

vectorFromList :: Num a => [a] -> Vector a
vectorFromList xs = (Vector xs)

skalarMult :: Num a => a -> Vector a -> Vector a
skalarMult x (Vector xs) = Vector (map (*x) xs)

vecAdd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
vecAdd (Vector xs) (Vector ys)
    | (length xs) == (length ys) = Just (Vector (zipWith (+) xs ys))
    | otherwise = Nothing

vecProd :: Num a => Vector a -> Vector a -> Maybe a  -- dot product
vecProd (Vector xs) (Vector ys)
    | (length xs) == (length ys) = Just (sum (zipWith (*) xs ys))
    | otherwise = Nothing

crossProd3 :: Num a => [a] -> [a] -> [a]
crossProd3 [x1,x2,x3] [y1,y2,y3] = [(x2*y3-x3*y2), (x3*y1-x1*y3), (x1*y2-x2*y1)]

crossProd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
crossProd (Vector xs) (Vector ys)
    | (length xs == 3) && (length ys == 3) = Just (Vector (crossProd3 xs ys))
    | otherwise = Nothing