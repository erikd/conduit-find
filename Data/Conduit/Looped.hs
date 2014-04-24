{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | Main entry point to the application.
module Data.Conduit.Looped where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Instances
import Data.Profunctor
import Prelude hiding ((.), id)

data Result m a b
    = Ignore
    | Keep b
    | Recurse (Looped m a b)
    | KeepAndRecurse b (Looped m a b)

instance Show a => Show (Result r m a) where
    show Ignore = "Ignore"
    show (Keep a) = "Keep " ++ show a
    show (Recurse _) = "Recurse"
    show (KeepAndRecurse a _) = "KeepAndRecurse " ++ show a

instance Functor m => Functor (Result m a) where
    fmap f (Keep a) = Keep (f a)
    fmap f (Recurse l) = Recurse (fmap f l)
    fmap f (KeepAndRecurse a l)  = KeepAndRecurse (f a) (fmap f l)

instance Functor m => Profunctor (Result m) where
    lmap _ (Keep a) = Keep a
    lmap f (Recurse l) = Recurse (lmap f l)
    lmap f (KeepAndRecurse a l)  = KeepAndRecurse a (lmap f l)
    rmap = fmap

instance Monad m => Monad (Result m a) where
    return = Keep
    Ignore >>= _ = Ignore
    Keep a >>= f = f a
    Recurse (Looped l) >>= f = Recurse (Looped $ \r -> liftM (>>= f) (l r))
    KeepAndRecurse a (Looped l) >>= f = case f a of
        Ignore -> Ignore
        Keep b -> KeepAndRecurse b (Looped $ \r -> liftM (>>= f) (l r))
        x@(Recurse (Looped m)) -> x
        x@(KeepAndRecurse b (Looped m)) -> x

newtype Looped m a b = Looped { runLooped :: a -> m (Result m a b) }

instance Functor m => Functor (Looped m a) where
    fmap f (Looped g) = Looped (fmap (fmap (fmap f)) g)

instance Functor m => Profunctor (Looped m) where
    lmap f (Looped k) = Looped (fmap (fmap (lmap f)) (k . f))
    rmap = fmap

instance Monad m => Monad (Looped m a) where
    return = Looped . const . return . return
    Looped f >>= k = Looped $ \a -> do
        r <- f a
        case r of
            Ignore -> return Ignore
            Keep b -> runLooped (k b) a
            Recurse l -> runLooped (l >>= k) a
            KeepAndRecurse b _ -> runLooped (k b) a

instance Monad m => Category (Looped m) where
    id = Looped $ \a -> return $ Keep a
    Looped f . Looped g = Looped $ \a -> do
          r <- g a
          case r of
            Ignore -> return Ignore
            Keep b -> do
                r' <- f b
                case r' of
                    Ignore -> return Ignore
                    Keep c -> return $ Keep c
                    Recurse (Looped l) -> return $ Recurse (Looped l . Looped g)
                    KeepAndRecurse b (Looped l) -> return $ KeepAndRecurse b (Looped l . Looped g)
            Recurse (Looped l) -> return $ Recurse (Looped f . Looped l)
            KeepAndRecurse b (Looped l) -> do
                r' <- f b
                case r' of
                    Ignore -> return Ignore
                    Keep c -> return $ Keep c
                    Recurse (Looped m) -> return $ Recurse (Looped m . Looped l)
                    KeepAndRecurse b (Looped m) -> return $ KeepAndRecurse b (Looped m . Looped l)

instance Monad m => Arrow (Looped m) where
    arr f = Looped $ return . Keep . f
    first (Looped f) = Looped $ \(a, c) -> do
        r <- f a
        return $ case r of
            Ignore -> Ignore
            Keep b -> Keep (b, c)
            Recurse l -> Recurse (first l)
            KeepAndRecurse b l -> KeepAndRecurse (b, c) (first l)

evens :: Monad m => Looped m Int Int
evens = Looped $ \x ->
    return $ if even x
             then KeepAndRecurse x evens
             else Recurse evens

tens :: Monad m => Looped m Int Int
tens = Looped $ \x ->
    return $ if x `mod` 10 == 0
             then KeepAndRecurse x tens
             else Ignore

apply :: Looped m a b -> a -> Looped m a b
apply l x = Looped $ const $ runLooped l x

applyPredicate :: Monad m => Looped m a b -> a -> (b -> m ()) -> (Looped m a b -> m ()) -> m ()
applyPredicate l x f g = do
    r <- runLooped l x
    case r of
        Ignore -> return ()
        Keep a -> f a
        Recurse m -> g m
        KeepAndRecurse a m -> f a >> g m

-- | The main entry point.
main :: IO ()
main =
    let x k = match k 52 print (\l -> x l)
    in x pr
  where
    pr = do
       x <- evens
       if x == 52
           then apply tens 100
           else return 5
