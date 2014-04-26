{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cond
    ( Result(..), toResult, fromResult
    , CondT(..), runCondT, Cond, runCond
    , if_, ifM_, apply, test
    , reject, rejectAll, norecurse
    , or_, (||:), and_, (&&:), not_, prune
    , traverseRecursively
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.State (StateT(..), withStateT, evalStateT)
import Data.Foldable
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Prelude hiding (foldr1)

-- | 'Result' is an enriched 'Maybe' type which additionally specifies whether
--   recursion should occur from the given input, and if so, how such
--   recursion should be done.  It is isomorphic to:
--
-- @
-- type Result a m b = (Maybe b, Maybe (Maybe (CondT a m b)))
-- @
data Result a m b = Ignore
                  | Keep b
                  | RecurseOnly (Maybe (CondT a m b))
                  | KeepAndRecurse b (Maybe (CondT a m b))

instance Show b => Show (Result a m b) where
    show Ignore               = "Ignore"
    show (Keep a)             = "Keep " ++ show a
    show (RecurseOnly _)      = "RecurseOnly"
    show (KeepAndRecurse a _) = "KeepAndRecurse " ++ show a

instance Monad m => Functor (Result a m) where
    fmap _ Ignore               = Ignore
    fmap f (Keep a)             = Keep (f a)
    fmap f (RecurseOnly l)      = RecurseOnly (liftM (fmap f) l)
    fmap f (KeepAndRecurse a l) = KeepAndRecurse (f a) (liftM (fmap f) l)

instance Semigroup (Result a m b) where
    Ignore        <> _                  = Ignore
    _             <> Ignore             = Ignore
    RecurseOnly _ <> Keep _             = Ignore
    RecurseOnly _ <> KeepAndRecurse _ m = RecurseOnly m
    RecurseOnly m <> _                  = RecurseOnly m
    Keep _        <> RecurseOnly _      = Ignore
    _             <> RecurseOnly m      = RecurseOnly m
    _             <> Keep b             = Keep b
    Keep _        <> KeepAndRecurse b _ = Keep b
    _             <> KeepAndRecurse b m = KeepAndRecurse b m

instance Monoid b => Monoid (Result a m b) where
    mempty        = KeepAndRecurse mempty Nothing
    x `mappend` y = x <> y

recurse :: Result a m b
recurse = RecurseOnly Nothing

accept :: b -> Result a m b
accept = flip KeepAndRecurse Nothing

toResult :: Monad m => Maybe a -> forall r. Result r m a
toResult Nothing  = recurse
toResult (Just a) = accept a

fromResult :: Monad m => forall r. Result r m a -> Maybe a
fromResult Ignore               = Nothing
fromResult (Keep a)             = Just a
fromResult (RecurseOnly _)      = Nothing
fromResult (KeepAndRecurse a _) = Just a

-- | 'CondT" is an arrow that maps from 'a' to @m b@, but only if 'a'
--   satisfies certain conditions.  It is a Monad, meaning each condition
--   stated must be satisfied for the map to succeed (in the spirit of the
--   'Maybe' short-circuiting Monad).  In fact, 'CondT' is nearly equivalent
--   to @StateT a (MaybeT m) b@, with some additional complexity for
--   performing recursive iterations (see the 'Result' type above).
--
--   You can promote functions of type @a -> m (Maybe b)@ into 'CondT' using
--   'apply'.  Pure functions @a -> Bool@ are lifted with 'if_', and
--   @a -> m Bool@ with 'ifM_'.  In effect, @if_ f@ is the same as
--   @ask >>= guard . f@.
--
--   Here is a trivial example:
--
-- @
--   flip runCondT 42 $ do
--     if_ even
--     liftIO $ putStrLn "42 must be even to reach here"
--
--     if_ odd <|> if_ even
--     if_ even &&: if_ (== 42)
--     if_ ((== 0) . (`mod` 6))
-- @
--
--   'CondT' is typically invoked using 'runCondT', in which case it maps 'a'
--   to 'Maybe b'.  It can also be run with 'applyCondT', which does case
--   analysis on the final 'Result' type, specifying how recursion should be
--   performed from the given 'a' value.  This is useful when applying
--   Conduits to structural traversals, and was the motivation behind this
--   type.
newtype CondT a m b = CondT { getCondT :: StateT a m (Result a m b) }

type Cond a = CondT a Identity

instance Monad m => Semigroup (CondT a m b) where
    (<>) = (>>)

instance Monad m => Monoid (CondT a m a) where
    mempty  = ask
    mappend = (<>)

instance Monad m => Functor (CondT a m) where
    fmap f (CondT g) = CondT (liftM (fmap f) g)

instance Monad m => Applicative (CondT a m) where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (CondT a m) where
    return = CondT . return . accept
    fail _ = mzero
    CondT f >>= k = CondT $ do
        r <- f
        case r of
            Ignore -> return Ignore
            Keep b -> do
                n <- getCondT (k b)
                return $ case n of
                    RecurseOnly _      -> Ignore
                    KeepAndRecurse c _ -> Keep c
                    _                  -> n
            RecurseOnly l -> return $ RecurseOnly (fmap (>>= k) l)
            KeepAndRecurse b _ -> getCondT (k b)

instance Monad m => MonadReader a (CondT a m) where
    ask               = CondT $ liftM accept get
    local f (CondT m) = CondT $ withStateT f m
    reader f          = f <$> ask

instance Monad m => MonadState a (CondT a m) where
    get     = CondT $ accept `liftM` get
    put s   = CondT $ accept `liftM` put s
    state f = CondT $ state (fmap (first accept) f)

instance Monad m => MonadPlus (CondT a m) where
    mzero = CondT $ return recurse
    CondT f `mplus` CondT g = CondT $ do
        r <- f
        case r of
            x@(Keep _) -> return x
            x@(KeepAndRecurse _ _) -> return x
            _ -> g

instance MonadThrow m => MonadThrow (CondT a m) where
  throwM = CondT . throwM

instance MonadCatch m => MonadCatch (CondT a m) where
    catch (CondT m) c = CondT $ m `catch` \e -> getCondT (c e)
    mask a = CondT $ mask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT
    uninterruptibleMask a =
        CondT $ uninterruptibleMask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT

instance MonadBase b m => MonadBase b (CondT a m) where
    liftBase m = CondT $ accept `liftM` liftBase m

instance MonadIO m => MonadIO (CondT a m) where
    liftIO m = CondT $ accept `liftM` liftIO m

instance MonadTrans (CondT a) where
    lift m = CondT $ accept `liftM` lift m

instance MonadBaseControl b m => MonadBaseControl b (CondT r m) where
    newtype StM (CondT r m) a =
        CondTStM { unCondTStM :: StM m (Result r m a, r) }
    liftBaseWith f = CondT $ StateT $ \s ->
        liftM (\x -> (accept x, s)) $ liftBaseWith $ \runInBase -> f $ \k ->
            liftM CondTStM $ runInBase $ runStateT (getCondT k) s
    restoreM = CondT . StateT . const . restoreM . unCondTStM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

runCondT :: Monad m => CondT a m b -> a -> m (Maybe b)
runCondT (CondT f) a = do
    r <- evalStateT f a
    return $ case r of
        Ignore             -> Nothing
        Keep b             -> Just b
        RecurseOnly _      -> Nothing
        KeepAndRecurse b _ -> Just b

runCond :: Cond a b -> a -> Maybe b
runCond = (runIdentity .) . runCondT

if_ :: Monad m => (a -> Bool) -> CondT a m ()
if_ f = ask >>= guard . f

ifM_ :: Monad m => (a -> m Bool) -> CondT a m ()
ifM_ f = ask >>= lift . f >>= guard

apply :: Monad m => (a -> m (Maybe b)) -> CondT a m b
apply f = CondT $ liftM toResult . lift . f =<< get

test :: Monad m => CondT a m b -> a -> m Bool
test = (liftM isJust .) . runCondT

-- | 'reject' rejects the current entry, but allows recursion.  This is the
--   same as 'mzero'.
reject :: Monad m => CondT a m b
reject = mzero

-- | 'rejectAll' rejects the entry and all of its descendents.
rejectAll :: Monad m => CondT a m b
rejectAll = CondT $ return Ignore

or_ :: Monad m => NonEmpty (CondT a m b) -> CondT a m b
or_ = foldr1 mplus

infixr 3 &&:
(||:) :: Monad m => CondT a m b -> CondT a m b -> CondT a m b
(||:) = mplus

and_ :: Monad m => [CondT a m b] -> CondT a m [b]
and_ = foldl' (\acc x -> (:) <$> x <*> acc) (return [])

infixr 2 ||:
(&&:) :: Monad m => CondT a m b -> CondT a m c -> CondT a m (b, c)
(&&:) = liftM2 (,)

-- | 'not_' inverts the meaning of the given predicate while preserving
--   recursion.
not_ :: Monad m => CondT a m () -> CondT a m ()
not_ (CondT f) = CondT $ go `liftM` f
  where
    go Ignore               = accept ()
    go (Keep _)             = recurse
    go (RecurseOnly _)      = accept ()
    go (KeepAndRecurse _ _) = recurse

-- | 'prune' is like 'not_', but does not preserve recursion.  It should be read
--   as "prune this entry and all its descendents if the predicate matches".
--   It is the same as @x ||: rejectAll@.
prune :: Monad m => CondT a m () -> CondT a m ()
prune (CondT f) = CondT $ go `liftM` f
  where
    go Ignore               = accept ()
    go (Keep _)             = Ignore
    go (RecurseOnly _)      = accept ()
    go (KeepAndRecurse _ _) = Ignore

-- | 'norecurse' indicates that recursion should not be performed on the current
--   item.  Note that this library doesn't perform any actual recursion; that
--   is up to the consumer of the final 'Result' value, typically
--   'applyCondT'.
norecurse :: Monad m => CondT a m ()
norecurse = CondT $ return (Keep ())

traverseRecursively :: (Monad m, Monad n)
                    => a
                    -> CondT a m b
                    -> (a -> b -> n ())
                    -> (forall x. m x -> n x)
                    -> (a -> (a -> n ()) -> n ())
                    -> n ()
traverseRecursively a c f trans getChildren = do
    r <- trans $ evalStateT (getCondT c) a
    case r of
        Ignore             -> return ()
        Keep b             -> f a b
        RecurseOnly m      -> descend (fromMaybe c m)
        KeepAndRecurse b m -> f a b >> descend (fromMaybe c m)
  where
    descend next = getChildren a $ \child ->
        traverseRecursively child next f trans getChildren
