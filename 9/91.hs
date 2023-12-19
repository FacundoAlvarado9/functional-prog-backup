data Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> smth =  fmap f smth

instance Monad Identity where
    Identity a >>= g = g a
    return = pure -- diff return / pure -> pure is the monad operator -> Both lift your value up to the monad context

-----------------
data CountBinds a = CountBinds (Integer, a)

instance Functor CountBinds where
    fmap f (CountBinds (i, a)) = CountBinds (i, (f a))

instance Applicative CountBinds where
    pure a = CountBinds (0, a)
    (CountBinds (i, f)) <*> smth = fmap f smth -- not correct: should sum the counts
    -- CountBinds (count1+count2, f smth)

instance Monad CountBinds where
    CountBinds (i, a) >>= g = let CountBinds (i', b) = g a in CountBinds (i + i' + 1, b)
    -- note to self: g :: (a -> m b)
    -- (i + i' + 1) where i are the steps we had until now,
    -- i' are the numner of binds during the application of g
    -- and we sum 1 for this step. (correct?)

binds :: CountBinds a -> Integer
binds (CountBinds (i, a)) = i

two :: CountBinds Int
two = do
    x <- return 1
    y <- return 2
    return (x + y)

test = binds two