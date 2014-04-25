{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Conduit.Find.Looped where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Monoid hiding ((<>))
import Data.Profunctor
import Data.Semigroup
import Prelude hiding ((.), id)

data Result m a b
    = Ignore
    | Keep b
    | RecurseOnly (Looped m a b)
    | KeepAndRecurse b (Looped m a b)

instance Show a => Show (Result r m a) where
    show Ignore = "Ignore"
    show (Keep a) = "Keep " ++ show a
    show (RecurseOnly _) = "RecurseOnly"
    show (KeepAndRecurse a _) = "KeepAndRecurse " ++ show a

instance Functor m => Functor (Result m a) where
    fmap _ Ignore = Ignore
    fmap f (Keep a) = Keep (f a)
    fmap f (RecurseOnly l) = RecurseOnly (fmap f l)
    fmap f (KeepAndRecurse a l)  = KeepAndRecurse (f a) (fmap f l)

instance Functor m => Profunctor (Result m) where
    lmap _ Ignore = Ignore
    lmap _ (Keep a) = Keep a
    lmap f (RecurseOnly l) = RecurseOnly (lmap f l)
    lmap f (KeepAndRecurse a l)  = KeepAndRecurse a (lmap f l)
    rmap = fmap

instance (Functor m, Monad m) => Applicative (Result m a) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Result m a) where
    return = Keep
    fail _ = Ignore
    Ignore >>= _ = Ignore
    Keep a >>= f = case f a of
        Ignore -> Ignore
        Keep b -> Keep b
        (RecurseOnly _) -> Ignore
        (KeepAndRecurse b _) -> Keep b
    RecurseOnly (Looped l) >>= f =
        RecurseOnly (Looped $ \r -> liftM (>>= f) (l r))
    KeepAndRecurse a _ >>= f = f a

instance Semigroup (Result m a a) where
    Ignore <> _ = Ignore
    _ <> Ignore = Ignore
    RecurseOnly m <> _ = RecurseOnly m
    _ <> RecurseOnly m = RecurseOnly m
    _ <> Keep b = Keep b
    Keep _ <> KeepAndRecurse b _ = Keep b
    KeepAndRecurse _ _ <> KeepAndRecurse b m = KeepAndRecurse b m

instance Monoid (Result m a a) where
    mempty = Ignore
    x `mappend` y = x <> y

instance Monad m => MonadPlus (Result m a) where
    mzero = Ignore
    Ignore `mplus` _ = Ignore
    _ `mplus` Ignore = Ignore
    RecurseOnly m `mplus` _ = RecurseOnly m
    _ `mplus` RecurseOnly m = RecurseOnly m
    _ `mplus` Keep b = Keep b
    Keep _ `mplus` KeepAndRecurse b _ = Keep b
    KeepAndRecurse _ _ `mplus` KeepAndRecurse b m = KeepAndRecurse b m

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
    fail _ = mzero
    Looped f >>= k = Looped $ \a -> do
        r <- f a
        case r of
            Ignore -> return Ignore
            Keep b -> runLooped (k b) a
            RecurseOnly l -> runLooped (l >>= k) a
            KeepAndRecurse b _ -> runLooped (k b) a

instance Monad m => Category (Looped m) where
    id = matchAll
    Looped f . Looped g = Looped $ \a -> do
          r <- g a
          case r of
            Ignore -> return Ignore
            Keep b -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep c -> Keep c
                    RecurseOnly _ -> Ignore
                    KeepAndRecurse c _ -> Keep c
            RecurseOnly (Looped l) ->
                return $ RecurseOnly (Looped f . Looped l)
            KeepAndRecurse b (Looped l) -> do
                r' <- f b
                return $ case r' of
                    Ignore -> Ignore
                    Keep c -> Keep c
                    RecurseOnly (Looped m) ->
                        RecurseOnly (Looped m . Looped l)
                    KeepAndRecurse c (Looped m) ->
                        KeepAndRecurse c (Looped m . Looped l)

instance Monad m => Arrow (Looped m) where
    arr f = Looped $ return . Keep . f
    first (Looped f) = Looped $ \(a, c) -> do
        r <- f a
        return $ case r of
            Ignore -> Ignore
            Keep b -> Keep (b, c)
            RecurseOnly l -> RecurseOnly (first l)
            KeepAndRecurse b l -> KeepAndRecurse (b, c) (first l)

instance MonadThrow m => MonadThrow (Looped m a) where
  throwM e = Looped $ const $ throwM e

instance MonadCatch m => MonadCatch (Looped m a) where
  catch (Looped m) c =
      Looped $ \r -> m r `catch` \e -> runLooped (c e) r
  mask a = Looped $ \e -> mask $ \u -> runLooped (a $ q u) e
    where q u (Looped b) = Looped (u . b)
  uninterruptibleMask a =
    Looped $ \e -> uninterruptibleMask $ \u -> runLooped (a $ q u) e
      where q u (Looped b) = Looped (u . b)

-- | Within a predicate block, 'consider' a different item than what is
--   currently being predicated upon.  This makes it possible to write custom
--   logic within the Monad instance for a predicate, such as in this
--   contrived example:
--
-- @
--   flip runLooped \"bar.hs\" $ do
--       x <- filename_ (== \"foo.hs\")
--       when (x /= \"\") $
--           consider \"baz.hs\" $
--               filename_ (== \"baz.hs\")
-- @
consider :: a -> Looped m a b -> Looped m a b
consider x l = Looped $ const $ runLooped l x

applyLooped :: (MonadTrans t, (Monad (t m)), Monad m, Show b)
            => Looped m a b -> a -> (b -> t m ())
            -> (Looped m a b -> t m ()) -> t m ()
applyLooped l x f g = do
    r <- lift $ runLooped l x
    case r of
        Ignore -> return ()
        Keep a -> f a
        RecurseOnly m -> g m
        KeepAndRecurse a m -> f a >> g m

testSingle :: (Monad m, Monoid c) => Looped m a b -> a -> (b -> m c) -> m c
testSingle l x f = do
    r <- runLooped l x
    case r of
        Ignore -> return mempty
        Keep a -> f a
        RecurseOnly _ -> return mempty
        KeepAndRecurse a _ -> f a

if_ :: Monad m => (a -> Bool) -> Looped m a a
if_ f = Looped $ \a ->
    return $ if f a
             then KeepAndRecurse a (if_ f)
             else RecurseOnly (if_ f)

ifM_ :: Monad m => (a -> m Bool) -> Looped m a a
ifM_ f = Looped $ \a -> do
    r <- f a
    return $ if r
             then KeepAndRecurse a (ifM_ f)
             else RecurseOnly (ifM_ f)

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
        RecurseOnly l -> return $ RecurseOnly l
        KeepAndRecurse _ _ -> g a

liftArrow :: Monad m => (a -> b) -> Looped m a b
liftArrow f = Looped $ \a -> return $ KeepAndRecurse (f a) (liftArrow f)

liftKleisli :: Monad m => (a -> m b) -> Looped m a b
liftKleisli f = Looped $ \a -> do
    r <- f a
    return $ KeepAndRecurse r (liftKleisli f)

lowerKleisli :: Monad m => Looped m a b -> a -> m b
lowerKleisli (Looped f) a = do
    r <- f a
    case r of
        Ignore -> fail "Ignore"
        Keep b -> return b
        RecurseOnly _ -> fail "RecurseOnly"
        KeepAndRecurse b _ -> return b

liftKleisliMaybe :: Monad m => (a -> m (Maybe b)) -> Looped m a b
liftKleisliMaybe f = Looped $ \a -> do
    r <- f a
    return $ case r of
        Nothing -> RecurseOnly (liftKleisliMaybe f)
        Just b -> KeepAndRecurse b (liftKleisliMaybe f)

lowerKleisliMaybe :: Monad m => Looped m a b -> a -> m (Maybe b)
lowerKleisliMaybe (Looped f) a = do
    r <- f a
    return $ case r of
        Ignore -> Nothing
        Keep b -> Just b
        RecurseOnly _ -> Nothing
        KeepAndRecurse b _ -> Just b

type Predicate m a = Looped m a a

instance (Functor m, Monad m) => Semigroup (Predicate m a) where
    Looped f <> Looped g = Looped $ \a -> do
        r <- f a
        case r of
            Ignore -> g a
            Keep b -> return $ Keep b
            RecurseOnly _ -> g a
            KeepAndRecurse b m -> return $ KeepAndRecurse b m

instance (Functor m, Monad m) => Monoid (Predicate m a) where
    mempty = let x = Looped (\a -> return $ KeepAndRecurse a x) in x
    f `mappend` g = f <> g

instance Monad m => MonadPlus (Looped m a) where
    mzero = ignoreAll
    Looped f `mplus` Looped g = Looped $ \a -> do
        r <- f a
        case r of
            Ignore -> g a
            Keep b -> return $ Keep b
            RecurseOnly _ -> g a
            KeepAndRecurse b m -> return $ KeepAndRecurse b m

-- | 'matchAll' is id in the 'Predicate' Category.
matchAll :: Monad m => Predicate m a
matchAll = Looped $ \entry -> return $ KeepAndRecurse entry matchAll

-- | 'ignore' rejects the current entry, but allows recursion.
ignore :: Monad m => Looped m a b
ignore = Looped $ const $ return $ RecurseOnly ignore

-- | 'ignoreAll' rejects every entry, and does not recurse.  This is the same
--   as 'mzero'.
ignoreAll :: Monad m => Looped m a b
ignoreAll = Looped $ const $ return Ignore

-- | 'not_' reverse the meaning of the given predicate, preserving recursion.
not_ :: MonadIO m => Predicate m a -> Predicate m a
not_ (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go a Ignore = Keep a
    go _ (Keep _) = Ignore
    go a (RecurseOnly l) = KeepAndRecurse a (not_ l)
    go _ (KeepAndRecurse _ l) = RecurseOnly (not_ l)

-- | 'prune' is much like 'not_', but does not preserve recursion.
prune :: MonadIO m => Predicate m a -> Predicate m a
prune (Looped f) = Looped (\a -> go a `liftM` f a)
  where
    go a Ignore = Keep a
    go _ (Keep _) = Ignore
    go a (RecurseOnly l) = KeepAndRecurse a (prune l)
    go _ (KeepAndRecurse _ _) = Ignore
