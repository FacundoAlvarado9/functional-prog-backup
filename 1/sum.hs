-- Note from class: Add case for negatives
suma:: Int -> Int
suma n = if n == 1 then 1 else n+suma(n-1)

digit :: Int -> Int
digit n
    | n == 0 = 0
    | n < 0 = -digit(negate n)
    | otherwise = (mod n 10) + digit(div n 10)
    
isPrefixOf :: String -> String -> Bool
isPrefixOf s t
     | null s = True
     | head s == head t = isPrefixOf (tail s) (tail t)
     | otherwise = False
    
-- isPrefixOf :: String -> String -> Bool
-- isPrefixOf [] t = True
-- isPrefixOf s [] = False
-- isPrefixOf (x:xs) (y:ys)
    -- | x == y = isPrefixOf xs ys
    -- | otherwise = False