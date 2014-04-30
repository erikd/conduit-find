module Data.Conduit.Find.EParIO where

import           Conduit
import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import qualified Control.Monad.Par as P hiding (runParIO)
import           Control.Monad.Par.IO
import           Control.Monad.Trans.Control

{-
This code is all from Simon Marlow.
-}

newtype EParIO a = E { unE :: ParIO (Either SomeException a) }

instance Monad EParIO where
  return a = E (return (Right a))
  E m >>= k = E $ do
    r <- m
    case r of
      Left e -> return (Left e)
      Right a -> unE (k a)

instance MonadIO EParIO where
  liftIO io = E $ liftIO (try io)

liftPar :: ParIO a -> EParIO a
liftPar p = E $ p >>= return . Right

type EVar a = IVar (Either SomeException a)

-- new :: EParIO (EVar a)
-- new = liftPar P.new

-- get :: EVar a -> EParIO a
-- get evar = E $ P.get evar

-- putResult :: EParIO a -> EVar a -> ParIO ()
-- putResult (E e) var = e >>= P.put_ var

gatherFrom :: (MonadIO m, MonadBaseControl IO m)
           => Int                -- ^ Size of the queue to create
           -> (TBQueue o -> m ()) -- ^ Action that generates output values
           -> Producer m o
gatherFrom size scatter = do
    chan   <- liftIO $ newTBQueueIO size
    worker <- lift $ async (scatter chan)
    lift . restoreM =<< gather worker chan
  where
    gather worker chan = do
        (xs, mres) <- liftIO $ atomically $ do
            xs <- whileM (not <$> isEmptyTBQueue chan) (readTBQueue chan)
            (xs,) <$> pollSTM worker
        Prelude.mapM_ yield xs
        case mres of
            Just (Left e)  -> liftIO $ throwIO (e :: SomeException)
            Just (Right r) -> return r
            Nothing        -> gather worker chan
