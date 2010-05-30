-- | Condition variables for Haskell built on top of MVars.

module Control.Concurrent.Condition
       (
         -- * The @Condition@ type.
         Condition
       , create
       , create'

         -- * Locking operations.
       , acquire
       , release

         -- * Waiting and notification.
         --
         -- $lockStateInfo
       , wait
       , notify
       , notifyAll
       ) where

import Control.Applicative ((<$>))

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, isEmptyChan)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, takeMVar, putMVar,
                                      isEmptyMVar)

import Control.Monad (unless)

import Data.Function (fix)

-- | Condition data type.
data Condition =
  Condition { lock    :: MVar ()
            , waiters :: Chan (MVar ()) }

-- | Creates a condition from a lock.
create :: MVar ()                -- ^ An 'MVar' to associate with condition.
       -> IO Condition
create lock = Condition lock
                    <$> newChan

-- | Creates a condition with hidden associated 'MVar' which can be accessed
-- using only 'acquire' and 'release' operations.
create' :: IO Condition
create' = newMVar () >>= create

-- | Acquire an underlying lock.
acquire :: Condition -> IO ()
acquire = takeMVar . lock

-- | Release an underlying lock.
release :: Condition -> IO ()
release cond = putMVar (lock cond) ()

-- $lockStateInfo
-- All the following operations /must/ be called only when the lock associated
-- with condition is acquired. This can be done either by calling
-- 'Control.Concurrent.MVar.takeMVar' on the 'MVar' provided to 'create'
-- function or by calling 'acquire' on condition.

-- | Wait until notified.
wait :: Condition -> IO ()
wait (Condition lock waiters) = do
  checkLock lock "Control.Concurrent.Condition.wait"

  waiterLock <- newEmptyMVar
  writeChan waiters waiterLock

  putMVar lock ()
  _ <- takeMVar waiterLock
  _ <- takeMVar lock

  return ()

-- | Wake up one of the threads waiting on the condition.
-- Does not release an associated lock.
notify :: Condition -> IO ()
notify (Condition lock waiters) = do
  checkLock lock "Control.Concurrent.Condition.notify"

  empty <- isEmptyChan waiters
  unless empty $ do
    waiterLock <- readChan waiters
    putMVar waiterLock ()

-- | Wake up all of the thread waiting on the condition.
-- Does not release an associated lock.
-- NB: At the time this function is unfair and may cause starvation.
notifyAll :: Condition -> IO ()
notifyAll (Condition lock waiters) = do
  checkLock lock "Control.Concurrent.Condition.notifyAll"

  fix $ \loop -> do
    empty <- isEmptyChan waiters
    unless empty $ do
      waiterLock <- readChan waiters
      putMVar waiterLock ()

      loop

-- | Performs a check to ensure that a lock is acquired. If not then error
-- is issued. Used to check correctness of 'wait', 'notify' and 'notifyAll'
-- calls.
checkLock :: MVar a             -- ^ A lock to check.
          -> String             -- ^ Information added to the error message.
          -> IO ()
checkLock lock info = do
  empty <- isEmptyMVar lock
  unless empty $ error (info ++ " : lock is not acquired.")
