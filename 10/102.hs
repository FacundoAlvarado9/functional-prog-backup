newtype EitherTransformer m a c = EitherTransformer { runEitherTransformer :: m (Either a c) }

instance Monad m => Applicative (EitherTransformer m a) where
    pure c = EitherTransformer $ return (Right c)
    --(<*>) :: EitherTransformer m a (c->d) -> EitherTransformer m a c -> EitherTransformer m a d
    EitherTransformer func <*> EitherTransformer arg = EitherTransformer $ do
        mf <- func -- mf :: ( Either a (c->d) )
        case mf of
            Left e -> return (Left e) -- e :: a
            Right k -> do
                mv <- arg -- mv :: ( Either a c )
                case mv of
                    Left e -> return (Left e)
                    Right x -> return (Right (k x)) -- :: m (Either) 

instance Monad m => Functor (EitherTransformer m a) where
    fmap f = EitherTransformer . fmap (fmap f) . runEitherTransformer

instance Monad m => Monad (EitherTransformer m a) where
    --return = EitherTransformer . return . return
    return = pure
    -- (>>=) :: EitherTransformer m a c -> (c -> EitherTranformer m a d) -> EitherTransoformer m a d
    x >>= f = EitherTransformer $ do 
        either_value <- runEitherTransformer x -- right side :: m (Either a c) => left side :: (Either a c)
        case either_value of
            Left a   -> return (Left a)
            Right c    -> runEitherTransformer (f c) -- (f c) :: EitherTransformer m a d => runEitherTransformer (f c) :: m (Either a d)