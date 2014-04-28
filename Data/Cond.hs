{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cond
    ( CondT(..), Cond
    , runCondT, applyCondT, runCond, applyCond
    , guard_, guardM_, apply, test, matches
    , if_, when_, unless_, or_, and_, not_
    , ignore, prune, reject, recurse
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad hiding (mapM_, sequence_)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Morph
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.State (StateT(..), withStateT, evalStateT)
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Prelude hiding (mapM_, foldr1, sequence_)

-- | 'Result' is an enriched 'Maybe' type which additionally specifies whether
--   recursion should occur from the given input, and if so, how such
--   recursion should be done.
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
    {-# INLINE fmap #-}

instance MFunctor (Result a) where
    hoist _ Ignore                 = Ignore
    hoist _ (Keep a)               = Keep a
    hoist nat (RecurseOnly l)      = RecurseOnly (fmap (hoist nat) l)
    hoist nat (KeepAndRecurse a l) = KeepAndRecurse a (fmap (hoist nat) l)
    {-# INLINE hoist #-}

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
    mempty  = KeepAndRecurse mempty Nothing
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

accept' :: b -> Result a m b
accept' = flip KeepAndRecurse Nothing

recurse' :: Result a m b
recurse' = RecurseOnly Nothing

toResult :: Monad m => Maybe a -> forall r. Result r m a
toResult Nothing  = recurse'
toResult (Just a) = accept' a

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
--   'apply'.  Pure functions @a -> Bool@ are lifted with 'guard_', and
--   @a -> m Bool@ with 'ifM_'.  In effect, @guard_ f@ is the same as
--   @ask >>= guard . f@.
--
--   Here is a trivial example:
--
-- @
--   flip runCondT 42 $ do
--     guard_ even
--     liftIO $ putStrLn "42 must be even to reach here"
--
--     guard_ odd <|> guard_ even
--     guard_ even &&: guard_ (== 42)
--     guard_ ((== 0) . (`mod` 6))
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

instance (Monad m, Semigroup b) => Semigroup (CondT a m b) where
    (<>) = liftM2 (<>)
    {-# INLINE (<>) #-}

instance (Monad m, Monoid b) => Monoid (CondT a m b) where
    mempty  = CondT $ return $ accept' mempty
    {-# INLINE mempty #-}
    mappend = liftM2 mappend
    {-# INLINE mappend #-}

instance Monad m => Functor (CondT a m) where
    fmap f (CondT g) = CondT (liftM (fmap f) g)
    {-# INLINE fmap #-}

instance Monad m => Applicative (CondT a m) where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (CondT a m) where
    return = CondT . return . accept'
    {-# INLINE return #-}
    fail _ = mzero
    {-# INLINE fail #-}
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
    ask               = CondT $ liftM accept' get
    {-# INLINE ask #-}
    local f (CondT m) = CondT $ withStateT f m
    {-# INLINE local #-}
    reader f          = liftM f ask
    {-# INLINE reader #-}

instance Monad m => MonadState a (CondT a m) where
    get     = CondT $ liftM accept' get
    {-# INLINE get #-}
    put s   = CondT $ liftM accept' $ put s
    {-# INLINE put #-}
    state f = CondT $ state (fmap (first accept') f)
    {-# INLINE state #-}

instance Monad m => Alternative (CondT a m) where
    empty = CondT $ return recurse'
    {-# INLINE empty #-}
    CondT f <|> CondT g = CondT $ do
        r <- f
        case r of
            x@(Keep _) -> return x
            x@(KeepAndRecurse _ _) -> return x
            _ -> g
    {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (CondT a m) where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance MonadThrow m => MonadThrow (CondT a m) where
    throwM = CondT . throwM
    {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (CondT a m) where
    catch (CondT m) c = CondT $ m `catch` \e -> getCondT (c e)
    mask a = CondT $ mask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT
    uninterruptibleMask a =
        CondT $ uninterruptibleMask $ \u -> getCondT (a $ q u)
      where q u = CondT . u . getCondT

instance MonadBase b m => MonadBase b (CondT a m) where
    liftBase m = CondT $ liftM accept' $ liftBase m
    {-# INLINE liftBase #-}

instance MonadIO m => MonadIO (CondT a m) where
    liftIO m = CondT $ liftM accept' $ liftIO m
    {-# INLINE liftIO #-}

instance MonadTrans (CondT a) where
    lift m = CondT $ liftM accept' $ lift m
    {-# INLINE lift #-}

instance MonadBaseControl b m => MonadBaseControl b (CondT r m) where
    newtype StM (CondT r m) a =
        CondTStM { unCondTStM :: StM m (Result r m a, r) }
    liftBaseWith f = CondT $ StateT $ \s ->
        liftM (\x -> (accept' x, s)) $ liftBaseWith $ \runInBase -> f $ \k ->
            liftM CondTStM $ runInBase $ runStateT (getCondT k) s
    restoreM = CondT . StateT . const . restoreM . unCondTStM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MFunctor (CondT a) where
    hoist nat (CondT m) = CondT $ hoist nat (liftM (hoist nat) m)
    {-# INLINE hoist #-}

runCondT :: Monad m => CondT a m b -> a -> m (Maybe b)
runCondT (CondT f) a = fromResult `liftM` evalStateT f a
{-# INLINE runCondT #-}

runCond :: Cond a b -> a -> Maybe b
runCond = (runIdentity .) . runCondT
{-# INLINE runCond #-}

-- | Case analysis of applying a condition to an input value.  Note that
--   function provided should recurse, calling applyCondT again, if the given
--   condition is Just, should it wish to.  Not result value is accumulated by
--   this function, meaning you should use 'WriterT' if you wish to do so
--   (unlike the pure variant, 'applyCond', which must accumulate a value to
--   have any meaning).
applyCondT :: Monad m
           => a
           -> CondT a m b
           -> (a -> Maybe b -> Maybe (CondT a m b) -> m ())
           -> m ()
applyCondT a c k = do
    (r, a') <- runStateT (getCondT c) a
    case r of
        Ignore              -> k a' Nothing  Nothing
        Keep b              -> k a' (Just b) Nothing
        RecurseOnly c'      -> k a' Nothing  (Just (fromMaybe c c'))
        KeepAndRecurse b c' -> k a' (Just b) (Just (fromMaybe c c'))

-- | Case analysis of applying a pure condition to an input value.  Note that
--   the user is responsible for recursively calling this function if the
--   resulting condition is a Just value.  Each step of the recursion must
--   return a Monoid so that a final result may be collected.
applyCond :: Monoid c
          => a
          -> Cond a b
          -> (a -> Maybe b -> Maybe (Cond a b) -> c)
          -> c
applyCond a c k = case runIdentity (runStateT (getCondT c) a) of
    (Ignore, a')              -> k a' Nothing  Nothing
    (Keep b, a')              -> k a' (Just b) Nothing
    (RecurseOnly c', a')      -> k a' Nothing  (Just (fromMaybe c c'))
    (KeepAndRecurse b c', a') -> k a' (Just b) (Just (fromMaybe c c'))

guard_ :: Monad m => (a -> Bool) -> CondT a m ()
guard_ f = ask >>= guard . f
{-# INLINE guard_ #-}

guardM_ :: Monad m => (a -> m Bool) -> CondT a m ()
guardM_ f = ask >>= lift . f >>= guard
{-# INLINE guardM_ #-}

test :: Monad m => CondT a m b -> a -> m Bool
test = (liftM isJust .) . runCondT
{-# INLINE test #-}

matches :: Monad m => CondT a m b -> CondT a m Bool
matches = fmap isJust . optional
{-# INLINE matches #-}

apply :: Monad m => (a -> m (Maybe b)) -> CondT a m b
apply f = CondT $ get >>= liftM toResult . lift . f
{-# INLINE apply #-}

if_ :: Monad m => CondT a m b -> CondT a m c -> CondT a m c -> CondT a m c
if_ c x y = CondT $ do
    r <- getCondT c
    getCondT $ case r of
        Ignore             -> y
        Keep _             -> x
        RecurseOnly _      -> y
        KeepAndRecurse _ _ -> x
{-# INLINE if_ #-}

when_ :: Monad m => CondT a m b -> CondT a m () -> CondT a m ()
when_ c x = if_ c x (return ())
{-# INLINE when_ #-}

unless_ :: Monad m => CondT a m b -> CondT a m () -> CondT a m ()
unless_ c = if_ c (return ())
{-# INLINE unless_ #-}

-- | Check whether at least one of the given conditions is true.  This is a
--   synonym for 'Data.Foldable.asum'.
or_ :: Monad m => [CondT a m b] -> CondT a m b
or_ = asum
{-# INLINE or_ #-}

-- | Check that all of the given conditions are true.  This is a synonym for
--   'Data.Foldable.sequence_'.
and_ :: Monad m => [CondT a m b] -> CondT a m ()
and_ = sequence_
{-# INLINE and_ #-}

-- | 'not_' inverts the meaning of the given predicate.
not_ :: Monad m => CondT a m b -> CondT a m ()
not_ c = when_ c ignore
{-# INLINE not_ #-}

-- | 'ignore' ignores the current entry, but allows recursion into its
--   descendents.  This is the same as 'mzero'.
ignore :: Monad m => CondT a m b
ignore = mzero
{-# INLINE ignore #-}

-- | 'prune' prevents recursion into the current entry's descendents, but does
--   not ignore the entry itself.
prune :: Monad m => CondT a m ()
prune = CondT $ return $ Keep ()
{-# INLINE prune #-}

-- | 'reject' is a synonym for both ignoring and pruning an entry. It is the
--   same as 'prune >> ignore'.
reject :: Monad m => CondT a m ()
reject = CondT $ return Ignore
{-# INLINE reject #-}

-- | 'recurse' changes the recursion predicate for any child elements.  For
--   example, the following file-finding predicate looks for all @*.hs@ files,
--   but under any @.git@ directory, only looks for a file named @config@:
--
-- @
-- if_ (name_ \".git\" \>\> directory)
--     (ignore \>\> recurse (name_ \"config\"))
--     (glob \"*.hs\")
-- @
--
-- If this code had used @recurse (glob \"*.hs\"))@ instead in the else case,
-- it would have meant that @.git@ is only looked for at the top-level of the
-- search (i.e., the top-most element).
recurse :: Monad m => CondT a m b -> CondT a m b
recurse c = CondT $ do
    r <- getCondT c
    return $ case r of
        Ignore             -> Ignore
        Keep b             -> Keep b
        RecurseOnly _      -> RecurseOnly (Just c)
        KeepAndRecurse b _ -> KeepAndRecurse b (Just c)
