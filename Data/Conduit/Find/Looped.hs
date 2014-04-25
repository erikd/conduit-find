{-# LANGUAGE TupleSections #-}

-- | Main entry point to the application.
module Data.Conduit.Find.Looped where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
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
    fmap _ Ignore = Ignore
    fmap f (Keep a) = Keep (f a)
    fmap f (Recurse l) = Recurse (fmap f l)
    fmap f (KeepAndRecurse a l)  = KeepAndRecurse (f a) (fmap f l)

instance Functor m => Profunctor (Result m) where
    lmap _ Ignore = Ignore
    lmap _ (Keep a) = Keep a
    lmap f (Recurse l) = Recurse (lmap f l)
    lmap f (KeepAndRecurse a l)  = KeepAndRecurse a (lmap f l)
    rmap = fmap

instance (Functor m, Monad m) => Applicative (Result m a) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Result m a) where
    return = Keep
    Ignore >>= _ = Ignore
    Keep a >>= f = case f a of
        Ignore -> Ignore
        Keep b -> Keep b
        (Recurse _) -> Ignore
        (KeepAndRecurse b _) -> Keep b
    Recurse (Looped l) >>= f =
        Recurse (Looped $ \r -> liftM (>>= f) (l r))
    KeepAndRecurse a _ >>= f = f a

newtype Looped m a b = Looped { runLooped :: a -> m (Result m a b) }

instance Functor m => Functor (Looped m a) where
    fmap f (Looped g) = Looped (fmap (fmap (fmap f)) g)

instance Functor m => Profunctor (Looped m) where
    lmap f (Looped k) = Looped (fmap (fmap (lmap f)) (k . f))
    rmap = fmap

instance (Functor m, Monad m) => Applicative (Looped m a) where
    pure = return
    (<*>) = ap

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
    id = let x = Looped $ \a -> return $ KeepAndRecurse a x in x
    Looped f . Looped g = Looped $ \a -> do
          r <- g a
          case r of
            Ignore -> return Ignore
            Keep b -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep c -> Keep c
                    Recurse _ -> Ignore
                    KeepAndRecurse c _ -> Keep c
            Recurse (Looped l) ->
                return $ Recurse (Looped f . Looped l)
            KeepAndRecurse b (Looped l) -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep c -> Keep c
                    Recurse (Looped m) ->
                        Recurse (Looped m . Looped l)
                    KeepAndRecurse c (Looped m) ->
                        KeepAndRecurse c (Looped m . Looped l)

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

applyPredicate :: (MonadTrans t, (Monad (t m)), Monad m, Show b)
               => Looped m a b -> a -> (b -> t m ())
               -> (Looped m a b -> t m ()) -> t m ()
applyPredicate l x f g = do
    r <- lift $ runLooped l x
    case r of
        Ignore -> return ()
        Keep a -> f a
        Recurse m -> g m
        KeepAndRecurse a m -> f a >> g m

testSingle :: (Monad m, Monoid c) => Looped m a b -> a -> (b -> m c) -> m c
testSingle l x f = do
    r <- runLooped l x
    case r of
        Ignore -> return mempty
        Keep a -> f a
        Recurse _ -> return mempty
        KeepAndRecurse a _ -> f a

liftLooped :: Monad m => (a -> m b) -> Looped m a b
liftLooped f = Looped $ \a -> do
    r <- f a
    return $ KeepAndRecurse r (liftLooped f)

if_ :: Monad m => (a -> Bool) -> Looped m a a
if_ f = Looped $ \a ->
    return $ if f a
             then KeepAndRecurse a (if_ f)
             else Recurse (if_ f)

ifM_ :: Monad m => (a -> m Bool) -> Looped m a a
ifM_ f = Looped $ \a -> do
    r <- f a
    return $ if r
             then KeepAndRecurse a (ifM_ f)
             else Recurse (ifM_ f)

or_ :: MonadIO m => Looped m a b -> Looped m a b -> Looped m a b
or_ (Looped f) (Looped g) = Looped $ \a -> do
    r <- f a
    case r of
        Keep b -> return $ Keep b
        KeepAndRecurse b l -> return $ KeepAndRecurse b l
        _ -> g a

and_ :: MonadIO m => Looped m a b -> Looped m a b -> Looped m a b
and_ (Looped f) (Looped g) = Looped $ \a -> do
    r <- f a
    case r of
        Ignore -> return Ignore
        Keep _ -> g a
        Recurse l -> return $ Recurse l
        KeepAndRecurse _ _ -> g a

not_ :: MonadIO m => Looped m a a -> Looped m a a
not_ (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go a Ignore = Keep a
    go _ (Keep _) = Ignore
    go a (Recurse l) = KeepAndRecurse a (not_ l)
    go _ (KeepAndRecurse _ l) = Recurse (not_ l)

prune :: MonadIO m => Looped m a a -> Looped m a a
prune (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go a Ignore = Keep a
    go _ (Keep _) = Ignore
    go a (Recurse l) = KeepAndRecurse a (prune l)
    go _ (KeepAndRecurse _ _) = Ignore

promote :: Monad m => (a -> m (Maybe b)) -> Looped m a b
promote f = Looped $ \a -> do
    r <- f a
    return $ case r of
        Nothing -> Recurse (promote f)
        Just b -> KeepAndRecurse b (promote f)

demote :: Monad m => Looped m a b -> a -> m (Maybe b)
demote (Looped f) a = do
    r <- f a
    return $ case r of
        Ignore -> Nothing
        Keep b -> Just b
        Recurse _ -> Nothing
        KeepAndRecurse b _ -> Just b
