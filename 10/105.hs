data InscribedFunc a = InscribedFunc a (a->a)

instance Functor InscribedFunc where
    -- fmap :: (a -> b) -> InscribedFunc a -> InscribedFunc b
    fmap f (InscribedFunc a g) = InscribedFunc ( f( g(a) ) ) id

instance Applicative InscribedFunc where
    pure value = (InscribedFunc value id)
    -- <*> :: InscribedFunc (a->b) -> InscribedFunc a -> InscribedFunc b
    (InscribedFunc f f2) <*> (InscribedFunc a g) = InscribedFunc (f(g(a))) id

instance Monad InscribedFunc where
    return = pure
    -- InscribedFunc a -> (a -> InscribedFunc b) -> InscribedFunc b
    (InscribedFunc value func) >>= f2 = f2 (func value)

newtype InscFncT m a = InscFncT {runInscFncT :: m (InscribedFunc a)}

instance Monad m => Functor (InscFncT m) where
    -- fmap :: (a -> b) -> InscFnT m a -> InscFnT m b
    fmap f = InscFncT . (fmap (fmap f)) . runInscFncT
    -- it works but:
        -- runInscFncT :: m (InscribedFunc a)
        -- return type of inner (fmap f) :: InscribedFunc b
        -- return type of outer fmap (fmap f) :: m (InscribedFunc b)
        -- Why do we need to apply the constructor InscFncT?
        -- Isn't "m (InscribedFunc b)" already InscFncT?

instance Monad m => Applicative (InscFncT m) where
    pure = InscFncT . return . return
    -- from right to left
        -- first return: a -> (InscribedFunc a)
        -- second return: (InscribedFunc a) -> m (InscribedFunc a)
        -- third: same question as above
    
    -- <*> :: InscFncT m (a->b) -> InscFncT m a -> InscFncT m b
    InscFncT func <*> InscFncT val = InscFncT $ do
        f <- func -- (InscribedFunc (a->b))
        v <- val -- (InscribedFunc a)
        return (f <*> v)

-- 2 Problems:

-- instance Monad m => MonadTrans (InscFncT m) where
--     lift = InscFncT . InscFncT -- wrong but class not in scope
        

instance Monad m => Monad (InscFncT m) where
    return = pure
    -- (>>=) :: InscFncT m a -> (a -> InscFncT m b) -> InscFncT m b
    InscFncT x >>= f = InscFncT $ do
        aux <- x -- aux :: InscribedFunc a
        return (fmap f aux)