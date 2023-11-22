import Numeric.Natural

data Component a = Component a Natural
type Storage a = [Component a]
data Product a b = Product a [(b, Int)]

instance (Show a) => Show (Component a) where
    show (Component a n) = "Component " ++ show a ++ show n

instance (Eq a) => Eq (Component a) where
    (Component d1 b) == (Component d2 y) = d1 == d2

--instance (Show a) => Show (Product a b) where
  --  show (Product a b) = "Product " ++ show a ++ show b

contains :: Eq a => Storage a -> a -> Maybe Natural
contains [] _ = Nothing -- if storage empty, then whatever I'm looking for is not there
contains ((Component x q):xs) a
    | x == a = (Just q) -- Comparing descriptors
    | otherwise = contains xs a

test = contains [(Component 'a' 2), (Component 'b' 5)] 'b'
testNothing = contains [(Component 'a' 2), (Component 'b' 5)] 'c'

-------- Store

store :: Eq a => Storage a -> a -> Natural -> Storage a
store [] a q = [(Component a q)] --if storage is empty, just save it
store ((Component x p):xs) a q
    | x == a = [(Component a (p+q))] ++ xs -- if found, add quantity and append rest of storage
    | otherwise = [(Component x p)] ++ (store xs a q) -- if not, save component and keep looking

testStore = store [(Component 'a' 2), (Component 'b' 5), (Component 'c' 7)] 'b' 2
testStoreNewComp = store [(Component 'a' 2), (Component 'b' 5), (Component 'c' 7)] 'd' 10
testStoreInitiallyEmpty = store [] 'g' 8

------ Remove
remove :: Eq a => Storage a -> a -> Natural -> Storage a
--guardar en vacio
remove [] a q = [] --if storage is empty, don't do anything
remove ((Component x p):xs) a q
    | x == a = if p >= q then [(Component a (p-q))] ++ xs else [(Component a 0)] ++ xs
    | otherwise = [(Component x p)] ++ (remove xs a q) -- if not, save component and keep looking

testRemove = remove [(Component 'a' 2), (Component 'b' 5), (Component 'c' 7)] 'b' 2
testRemove2 = remove [(Component 'a' 2), (Component 'b' 5), (Component 'c' 7)] 'a' 5


--------Producible
isProducible :: (Eq a, Eq b) => Product a b -> Storage a -> Bool
isProducible (Product a (x:xs)) [] = False -- Non-empty need for components, empty storage => Non producible
isProducible (Product a []) (y:ys) = True -- No components needed, is producible
isProducible (Product a (b:xs)) st
    | enoughOfCompInStorage b st = isProducible (Product a xs) st
    | otherwise = False

enoughOfCompInStorage :: (Eq a, Eq b) => (b,Int) -> Storage a -> Bool --Iterates over storage to see if there are enough of component with desc. a
enoughOfCompInStorage (b,q) [] = False
enoughOfCompInStorage (b,q) ((Component x e):xs)
    | b == x = ((toInteger e) >= (toInteger q))
    | otherwise = enoughOfCompInStorage (b,q) xs