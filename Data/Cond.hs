{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cond
    ( Result(..), toResult, fromResult
    , CondT(..), runCondT, Cond, runCond
    , guard_, guardM_, guardAll_, guardAllM_, apply, test
    , reject, rejectAll, prune, pruneWhen_
    , or_, (||:), and_, (&&:), not_
    , if_, when_
    , Iterator(..), recCondFoldMap
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
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

instance MFunctor (Result a) where
    hoist _ Ignore                 = Ignore
    hoist _ (Keep a)               = Keep a
    hoist nat (RecurseOnly l)      = RecurseOnly (fmap (hoist nat) l)
    hoist nat (KeepAndRecurse a l) = KeepAndRecurse a (fmap (hoist nat) l)

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

instance MFunctor (CondT a) where
    hoist nat (CondT m) = CondT $ hoist nat (liftM (hoist nat) m)

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

guard_ :: Monad m => (a -> Bool) -> CondT a m ()
guard_ f = ask >>= guard . f

guardAll_ :: Monad m => (a -> Bool) -> CondT a m ()
guardAll_ f = ask >>= (`unless` rejectAll)  . f

if_ :: Monad m => CondT a m b -> CondT a m c -> CondT a m c -> CondT a m c
if_ c x y = CondT $ do
    r <- getCondT c
    getCondT $ case r of
        Ignore             -> y
        Keep _             -> x
        RecurseOnly _      -> y
        KeepAndRecurse _ _ -> x

when_ :: Monad m => CondT a m b -> CondT a m () -> CondT a m ()
when_ c x = if_ c x (return ())

guardM_ :: Monad m => (a -> m Bool) -> CondT a m ()
guardM_ f = ask >>= lift . f >>= guard

guardAllM_ :: Monad m => (a -> m Bool) -> CondT a m ()
guardAllM_ f = ask >>= lift . f >>= \x -> unless x rejectAll

apply :: Monad m => (a -> m (Maybe b)) -> CondT a m b
apply f = CondT $ liftM toResult . lift . f =<< get

test :: Monad m => CondT a m b -> a -> m Bool
test = (liftM isJust .) . runCondT

-- | 'reject' rejects the current entry, but allows recursion into its
--   descendents.  This is the same as 'mzero'.
reject :: Monad m => CondT a m b
reject = mzero

rejectAll :: Monad m => CondT a m b
rejectAll = prune >> reject

-- | 'prune' prevents recursion into the current entry's descendent, but does
--   not reject the entry itself.
prune :: Monad m => CondT a m ()
prune = CondT $ return $ Keep ()

or_ :: Monad m => NonEmpty (CondT a m b) -> CondT a m b
or_ = foldr1 mplus

infixr 3 &&:
(||:) :: Monad m => CondT a m b -> CondT a m b -> CondT a m b
(||:) = mplus

and_ :: Monad m => [CondT a m b] -> CondT a m [b]
and_ = foldl' (\acc x -> (:) <$> x <*> acc) (return [])

infixr 2 ||:
(&&:) :: (Monad m, Semigroup b) => CondT a m b -> CondT a m b -> CondT a m b
(&&:) = liftM2 (<>)

-- | 'not_' inverts the meaning of the given predicate while preserving
--   recursion.
not_ :: Monad m => CondT a m b -> CondT a m ()
not_ c = when_ c reject

pruneWhen_ :: Monad m => CondT a m b -> CondT a m ()
pruneWhen_ c = when_ c rejectAll

newtype Iterator a m b = Iterator
    { runIterator :: (a -> (b -> m b) -> Iterator a m b -> m b) -> m b }

recCondFoldMap :: (MonadIO m, Monoid b, Show a, Show b)
               => CondT a m b
               -> Iterator a m b
               -> m b
recCondFoldMap cond iter = runIterator iter $ \x f next -> do
    liftIO $ putStrLn $ "recCondFoldMap Cond.hs:300.." ++ show x
    r <- evalStateT (getCondT cond) x
    liftIO $ putStrLn $ "recCondFoldMap Cond.hs:302.." ++ show r
    case r of
        Ignore -> return mempty
        Keep c -> f c
        RecurseOnly cond' ->
            recCondFoldMap (fromMaybe cond cond') next
        KeepAndRecurse c cond' ->
            liftM2 mappend (f c)
                (recCondFoldMap (fromMaybe cond cond') next)
