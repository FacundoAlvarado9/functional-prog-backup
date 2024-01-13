newtype EitherTransformer m a = EitherTransformer { runEitherTransformer :: m (Either String a) }

instance Monad m => Applicative (EitherTransformer m) where
    pure a = EitherTransformer $ return (Right a)
    --(<*>) :: EitherTransformer m (a->b) -> EitherTransformer m a -> EitherTransformer m b
    EitherTransformer f <*> EitherTransformer v = EitherTransformer $ do
        mf <- f
        case mf of
            Left e -> return (Left e)
            Right k -> do
                mv <- v
                case mv of
                    Left e -> return (Left e)
                    Right x -> return (Right (k x))

instance Monad m => Functor (EitherTransformer m) where
    fmap f = EitherTransformer . fmap (fmap f) . runEitherTransformer

instance Monad m => Monad (EitherTransformer m) where
    --return = EitherTransformer . return . return
    return = pure
    -- (>>=) :: EitherTransformer m a -> (a -> EitherTranformer m b) -> EitherTransoformer m b
    x >>= f = EitherTransformer $ do 
        either_value <- runEitherTransformer x -- I should get (Either String a)
        case either_value of
            Left msg   -> return (Left msg)
            Right a    -> runEitherTransformer (f a)