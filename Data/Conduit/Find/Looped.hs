{-# LANGUAGE TupleSections #-}

-- | Main entry point to the application.
module Data.Conduit.Find.Looped where

-- import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Profunctor
import Debug.Trace
import Prelude hiding ((.), id)

data Result m a b
    = Ignore
    | Keep b
    | Recurse b (Looped m a b)
    | KeepAndRecurse b (Looped m a b)

instance Show a => Show (Result r m a) where
    show Ignore = "Ignore"
    show (Keep a) = "Keep " ++ show a
    show (Recurse a _) = "Recurse " ++ show a
    show (KeepAndRecurse a _) = "KeepAndRecurse " ++ show a

instance Functor m => Functor (Result m a) where
    fmap _ Ignore = Ignore
    fmap f (Keep a) = Keep (f a)
    fmap f (Recurse a l) = Recurse (f a) (fmap f l)
    fmap f (KeepAndRecurse a l)  = KeepAndRecurse (f a) (fmap f l)

instance Functor m => Profunctor (Result m) where
    lmap _ Ignore = Ignore
    lmap _ (Keep a) = Keep a
    lmap f (Recurse a l) = Recurse a (lmap f l)
    lmap f (KeepAndRecurse a l)  = KeepAndRecurse a (lmap f l)
    rmap = fmap

-- instance (Functor m, Monad m) => Applicative (Result m a) where
--     pure = return
--     (<*>) = ap

-- instance Monad m => Monad (Result m a) where
--     return = Keep
--     Ignore >>= _ = Ignore
--     Keep a >>= f = case f a of
--         Ignore -> Ignore
--         Keep b -> Keep b
--         (Recurse _ _) -> Ignore
--         (KeepAndRecurse b _) -> Keep b
--     Recurse a l >>= f = case f a of
--         Ignore -> Ignore
--         Keep _ -> Ignore
--         x@(Recurse b m) -> x     -- jww (2014-04-24): Is this right?
--         KeepAndRecurse b m -> Recurse b m -- jww (2014-04-24): Should we compose?
--     KeepAndRecurse a _ >>= f = f a       -- jww (2014-04-24): Should we compose?

newtype Looped m a b = Looped { runLooped :: a -> m (Result m a b) }

instance Functor m => Functor (Looped m a) where
    fmap f (Looped g) = Looped (fmap (fmap (fmap f)) g)

instance Functor m => Profunctor (Looped m) where
    lmap f (Looped k) = Looped (fmap (fmap (lmap f)) (k . f))
    rmap = fmap

-- instance (Functor m, Monad m) => Applicative (Looped m a) where
--     pure = return
--     (<*>) = ap

-- instance Monad m => Monad (Looped m a) where
--     return = Looped . const . return . return
--     Looped f >>= k = Looped $ \a -> do
--         r <- f a
--         case r of
--             Ignore -> return Ignore
--             Keep b -> runLooped (k b) a
--             Recurse _ l -> runLooped (l >>= k) a
--             KeepAndRecurse b _ -> runLooped (k b) a

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
                    Recurse _ _ -> Ignore
                    KeepAndRecurse c _ -> Keep c
            Recurse b (Looped l) -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep _ -> Ignore
                    Recurse c (Looped m) ->
                        Recurse c (Looped m . Looped l)
                    KeepAndRecurse c (Looped m) ->
                        Recurse c (Looped m . Looped l)
            KeepAndRecurse b (Looped l) -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep c -> Keep c
                    Recurse c (Looped m) ->
                        Recurse c (Looped m . Looped l)
                    KeepAndRecurse c (Looped m) ->
                        KeepAndRecurse c (Looped m . Looped l)

instance Monad m => Arrow (Looped m) where
    arr f = Looped $ return . Keep . f
    first (Looped f) = Looped $ \(a, c) -> do
        r <- f a
        return $ case r of
            Ignore -> Ignore
            Keep b -> Keep (b, c)
            Recurse b l -> Recurse (b, c) (first l)
            KeepAndRecurse b l -> KeepAndRecurse (b, c) (first l)

evens :: Monad m => Looped m Int Int
evens = Looped $ \x ->
    return $ if even x
             then KeepAndRecurse x evens
             else Recurse x evens

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
    case (trace ("r: " ++ show (r)) $ r) of
        Ignore -> return ()
        Keep a -> f a
        Recurse _ m -> g m
        KeepAndRecurse a m -> f a >> g m

testSingle :: (Monad m, Monoid c) => Looped m a b -> a -> (b -> m c) -> m c
testSingle l x f = do
    r <- runLooped l x
    case r of
        Ignore -> return mempty
        Keep a -> f a
        Recurse _ _ -> return mempty
        KeepAndRecurse a _ -> f a

liftLooped :: Monad m => (a -> m b) -> Looped m a b
liftLooped f = Looped $ \a -> do
    r <- f a
    return $ KeepAndRecurse r (liftLooped f)

-- check_ :: Monad m => Looped m a a -> Bool -> (a -> m (Maybe a)) -> Looped m a a
-- check_ x@(Looped l) prune f = Looped (\a -> go a =<< l a)
--   where
--     go _ Ignore = return Ignore
--     go _ (Recurse b m) = return $ Recurse b m
--     go _ (Keep b) = checked b `liftM` f b
--     go _ (KeepAndRecurse b m) = return $ Recurse b m

--     checked b Nothing = if prune
--                         then Ignore
--                         else Recurse b (check_ x prune f)
--     checked _ (Just c) = KeepAndRecurse c (check_ x prune f)

if_ :: Monad m => (a -> Bool) -> Looped m a a
if_ f = Looped $ \a ->
    return $ if f a
             then trace ("if_ keepr") $ KeepAndRecurse a (if_ f)
             else trace ("if_ dropr") $ Recurse a (if_ f)

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
        Recurse b l -> return $ Recurse b l
        KeepAndRecurse _ _ -> g a

not_ :: MonadIO m => Looped m a a -> Looped m a a
not_ (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go a Ignore = trace ("not_ keep") $ Keep a
    go _ (Keep _) = trace ("not_ drop") $ Ignore
    go a (Recurse _ l) = trace ("not_ keepr") $ KeepAndRecurse a (not_ l)
    go a (KeepAndRecurse _ l) = trace ("not_ dropr") $ Recurse a (not_ l)

pruneIgnored :: MonadIO m => Looped m a a -> Looped m a a
pruneIgnored (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go _ Ignore = Ignore
    go _ x@(Keep _) = x
    go _ (Recurse _ _) = Ignore
    go _ (KeepAndRecurse b m) = KeepAndRecurse b (pruneIgnored m)
