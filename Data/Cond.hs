{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cond
    ( CondT(..), Cond

    -- * Executing CondT
    , runCondT, runCond, applyCondT, applyCond

    -- * Promotions
    , guardM, guard_, guardM_, apply, consider

    -- * Boolean logic
    , matches, if_, when_, unless_, or_, and_, not_

    -- * Basic conditionals
    , ignore, norecurse, prune

    -- * Helper functions
    , recurse, test
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

-- | 'Result' is an enriched 'Maybe' type which also specifies whether
--   recursion should occur from the given input, and if so, how such
--   recursion should be performed.
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
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Result a m b) where
    mempty  = KeepAndRecurse mempty Nothing
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

getResult :: Result a m b -> (Maybe b, Maybe (CondT a m b))
getResult Ignore               = (Nothing, Nothing)
getResult (Keep b)             = (Just b, Nothing)
getResult (RecurseOnly c)      = (Nothing, c)
getResult (KeepAndRecurse b c) = (Just b, c)

setRecursion :: CondT a m b -> Result a m b -> Result a m b
setRecursion _ Ignore               = Ignore
setRecursion _ (Keep b)             = Keep b
setRecursion c (RecurseOnly _)      = RecurseOnly (Just c)
setRecursion c (KeepAndRecurse b _) = KeepAndRecurse b (Just c)

accept' :: b -> Result a m b
accept' = flip KeepAndRecurse Nothing

recurse' :: Result a m b
recurse' = RecurseOnly Nothing

-- | Convert from a 'Maybe' value to its corresponding 'Result'.
--
-- >>> maybeToResult Nothing :: Result () Identity ()
-- RecurseOnly
-- >>> maybeToResult (Just ()) :: Result () Identity ()
-- KeepAndRecurse ()
maybeToResult :: Monad m => Maybe a -> Result r m a
maybeToResult Nothing  = recurse'
maybeToResult (Just a) = accept' a

maybeFromResult :: Monad m => Result r m a -> Maybe a
maybeFromResult Ignore               = Nothing
maybeFromResult (Keep a)             = Just a
maybeFromResult (RecurseOnly _)      = Nothing
maybeFromResult (KeepAndRecurse a _) = Just a

-- | 'CondT' is a kind of @StateT a (MaybeT m) b@, which uses a special
--   'Result' type instead of 'Maybe' to express whether recursion should be
--   performed from the item under consideration.  This is used to build
--   predicates that can guide recursive traversals.
--
-- Several different types may be promoted to 'CondT':
--
--   [@Bool@]                  Using 'guard'
--
--   [@m Bool@]                Using 'guardM'
--
--   [@a -> Bool@]              Using 'guard_'
--
--   [@a -> m Bool@]            Using 'guardM_'
--
--   [@a -> m (Maybe b)@]       Using 'apply'
--
--   [@a -> m (Maybe (b, a))@]  Using 'consider'
--
-- Here is a trivial example:
--
-- @
-- flip runCondT 42 $ do
--   guard_ even
--   liftIO $ putStrLn "42 must be even to reach here"
--   guard_ odd \<|\> guard_ even
--   guard_ (== 42)
-- @
--
-- If 'CondT' is executed using 'runCondT', it return a @Maybe b@ if the
-- predicate matched.  It can also be run with 'applyCondT', which does case
-- analysis on the 'Result', specifying how recursion should be performed from
-- the given 'a' value.
newtype CondT a m b = CondT { getCondT :: StateT a m (Result a m b) }

type Cond a = CondT a Identity

instance Show (CondT a m b) where
    show _ = "CondT"

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
    ask               = CondT $ gets accept'
    {-# INLINE ask #-}
    local f (CondT m) = CondT $ withStateT f m
    {-# INLINE local #-}
    reader f          = liftM f ask
    {-# INLINE reader #-}

instance Monad m => MonadState a (CondT a m) where
    get     = CondT $ gets accept'
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

instance MonadMask m => MonadMask (CondT a m) where
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
runCondT (CondT f) a = maybeFromResult `liftM` evalStateT f a
{-# INLINE runCondT #-}

runCond :: Cond a b -> a -> Maybe b
runCond = (runIdentity .) . runCondT
{-# INLINE runCond #-}

-- | Case analysis of applying a condition to an input value.  The result is a
--   pair whose first part is a pair of Maybes specifying if the input matched
--   and if recursion is expected from this value, and whose second part is
--   the (possibly) mutated input value.
applyCondT :: Monad m
           => a
           -> CondT a m b
           -> m ((Maybe b, Maybe (CondT a m b)), a)
applyCondT a cond = do
    -- Apply a condition to the input value, determining the result and the
    -- mutated value.
    (r, a') <- runStateT (getCondT cond) a

    -- Convert from the 'Result' to a pair of Maybes: one to specify if the
    -- predicate succeeded, and the other to specify if recursion should be
    -- performed.  If there is no sub-recursion specified, return 'cond'.
    return (fmap (Just . fromMaybe cond) (getResult r), a')
{-# INLINE applyCondT #-}

-- | Case analysis of applying a pure condition to an input value.  The result
--   is a pair whose first part is a pair of Maybes specifying if the input
--   matched and if recursion is expected from this value, and whose second
--   part is the (possibly) mutated input value.
applyCond :: a -> Cond a b -> ((Maybe b, Maybe (Cond a b)), a)
applyCond a cond = first (fmap (Just . fromMaybe cond) . getResult)
                       (runIdentity (runStateT (getCondT cond) a))
{-# INLINE applyCond #-}

guardM :: Monad m => m Bool -> CondT a m ()
guardM m = lift m >>= guard
{-# INLINE guardM #-}

guard_ :: Monad m => (a -> Bool) -> CondT a m ()
guard_ f = asks f >>= guard
{-# INLINE guard_ #-}

guardM_ :: Monad m => (a -> m Bool) -> CondT a m ()
guardM_ f = ask >>= lift . f >>= guard
{-# INLINE guardM_ #-}

apply :: Monad m => (a -> m (Maybe b)) -> CondT a m b
apply f = CondT $ get >>= liftM maybeToResult . lift . f
{-# INLINE apply #-}

consider :: Monad m => (a -> m (Maybe (b, a))) -> CondT a m b
consider f = CondT $ do
    mres <- lift . f =<< get
    case mres of
        Nothing      -> return Ignore
        Just (b, a') -> put a' >> return (accept' b)
{-# INLINE consider #-}

-- | Return True or False depending on whether the given condition matches or
--   not.  This differs from simply stating the condition in that it itself
--   always succeeds.
--
-- >>> flip runCond "foo.hs" $ matches (guard =<< asks (== "foo.hs"))
-- Just True
-- >>> flip runCond "foo.hs" $ matches (guard =<< asks (== "foo.hi"))
-- Just False
matches :: Monad m => CondT a m b -> CondT a m Bool
matches = fmap isJust . optional
{-# INLINE matches #-}

-- | A variant of ifM which branches on whether the condition succeeds or not.
--   Note that @if_ x@ is equivalent to @ifM (matches x)@, and is provided
--   solely for convenience.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ if_ good (return "Success") (return "Failure")
-- Just "Success"
-- >>> flip runCond "foo.hs" $ if_ bad (return "Success") (return "Failure")
-- Just "Failure"
if_ :: Monad m => CondT a m r -> CondT a m b -> CondT a m b -> CondT a m b
if_ c x y =
    CondT $ getCondT . maybe y (const x) . maybeFromResult =<< getCondT c
{-# INLINE if_ #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition passes, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ when_ good ignore
-- Nothing
-- >>> flip runCond "foo.hs" $ when_ bad ignore
-- Just ()
when_ :: Monad m => CondT a m r -> CondT a m () -> CondT a m ()
when_ c x = if_ c (void x) (return ())
{-# INLINE when_ #-}

-- | 'when_' is just like 'when', except that it executes the body if the
--   condition fails, rather than based on a Bool value.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ unless_ bad ignore
-- Nothing
-- >>> flip runCond "foo.hs" $ unless_ good ignore
-- Just ()
unless_ :: Monad m => CondT a m r -> CondT a m () -> CondT a m ()
unless_ c = if_ c (return ()) . void
{-# INLINE unless_ #-}

-- | Check whether at least one of the given conditions is true.  This is a
--   synonym for 'Data.Foldable.asum'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ or_ [bad, good]
-- Just ()
-- >>> flip runCond "foo.hs" $ or_ [bad]
-- Nothing
or_ :: Monad m => [CondT a m b] -> CondT a m b
or_ = asum
{-# INLINE or_ #-}

-- | Check that all of the given conditions are true.  This is a synonym for
--   'Data.Foldable.sequence_'.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ and_ [bad, good]
-- Nothing
-- >>> flip runCond "foo.hs" $ and_ [good]
-- Just ()
and_ :: Monad m => [CondT a m b] -> CondT a m ()
and_ = sequence_
{-# INLINE and_ #-}

-- | 'not_' inverts the meaning of the given predicate.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> flip runCond "foo.hs" $ not_ bad >> return "Success"
-- Just "Success"
-- >>> flip runCond "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- Nothing
not_ :: Monad m => CondT a m b -> CondT a m ()
not_ c = when_ c ignore
{-# INLINE not_ #-}

-- | 'ignore' ignores the current entry, but allows recursion into its
--   descendents.  This is the same as 'mzero'.
ignore :: Monad m => CondT a m b
ignore = mzero
{-# INLINE ignore #-}

-- | 'norecurse' prevents recursion into the current entry's descendents, but
--   does not ignore the entry itself.
norecurse :: Monad m => CondT a m ()
norecurse = CondT $ return $ Keep ()
{-# INLINE norecurse #-}

-- | 'prune' is a synonym for both ignoring an entry and its descendents. It
--   is the same as @ignore >> norecurse@.
prune :: Monad m => CondT a m b
prune = CondT $ return Ignore
{-# INLINE prune #-}

-- | 'recurse' changes the recursion predicate for any child elements.  For
--   example, the following file-finding predicate looks for all @*.hs@ files,
--   but under any @.git@ directory looks only for a file named @config@:
--
-- @
-- if_ (name_ \".git\" \>\> directory)
--     (ignore \>\> recurse (name_ \"config\"))
--     (glob \"*.hs\")
-- @
--
-- NOTE: If this code had used @recurse (glob \"*.hs\"))@ instead in the else
-- case, it would have meant that @.git@ is only looked for at the top-level
-- of the search (i.e., the top-most element).
recurse :: Monad m => CondT a m b -> CondT a m b
recurse c = CondT $ setRecursion c `liftM` getCondT c
{-# INLINE recurse #-}

-- | A specialized variant of 'runCondT' that simply returns True or False.
--
-- >>> let good = guard_ (== "foo.hs") :: Cond String ()
-- >>> let bad  = guard_ (== "foo.hi") :: Cond String ()
-- >>> runIdentity $ test "foo.hs" $ not_ bad >> return "Success"
-- True
-- >>> runIdentity $ test "foo.hs" $ not_ good >> return "Shouldn't reach here"
-- False
test :: Monad m => a -> CondT a m b -> m Bool
test = (liftM isJust .) . flip runCondT
{-# INLINE test #-}
