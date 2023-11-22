safeDiv :: Maybe Double -> Maybe Double -> Maybe Double
safeDiv a b = case (a,b) of
    (Just x, Just y)
        | y == 0 -> Nothing
        | otherwise -> Just (x/y)
    (Just x, Nothing) -> Nothing
    (Nothing, Just y) -> Nothing

safeRoot :: Maybe Double -> Maybe Double
safeRoot a = case a of
    (Just x)
        | x < 0 -> Nothing
        | otherwise -> Just (sqrt(x))
    Nothing -> Nothing

--- Allows us to deal with errors or exceptional cases without resorting to drastic measures (e.g. errors)
--- or even "ugly" outputs [e.g. sqrt (negate 4)]

--- ---
--- Error handling: Handling exceptions without interrupting the code
--- Undefined cases or exceptional cases are explicit in the code signature
--- It is a monad -> Already implemented. Not necessary to implement, for example for Int or Double.


--- 2.3. We can have enumerations of just one value.
--- data () = (), in comparison to data Bool = True | False
--- usability: have an undefined

--- n6: Mixed Items with already-implemented list
--- _lCount ((MixedString val):list) s i b = _lCount list (s+1) i b

--- listType [] = []
--- listType ((ItemType v):l) = v : ListType ...