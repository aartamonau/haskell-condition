-- | Condition variables for Haskell built on top of MVars.

module Control.Concurrent.Condition
       (
         -- * @Condition@ type.
         Condition

         -- * Creation.
       , create
       , create'

         -- * Locking operations.
       , acquire
       , release

         -- * Waiting and notification.
         --
         -- $lockStateInfo
       , wait
       , waitFor
       , notify
       , notifyAll
       ) where

import Control.Applicative ((<$>))

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, isEmptyChan)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
                                      takeMVar, putMVar, readMVar, modifyMVar_,
                                      isEmptyMVar)

import Control.Monad (unless)

import Data.Function (fix)
import Data.Maybe (isNothing, fromJust)

import System.Timeout (timeout)

-- | Possible states for 'Waiter'.
data WaiterState
  = Waiting                     -- ^ A waiter is waiting for notification
                                -- indefinetly long.
  | WaitingTimeout              -- ^ A waiter is waiting for notification
                                -- but can't be interrupted by timeout.
  | Aborted                     -- ^ A waiter has aborted its waiting due to
                                -- timeout.

-- | Represents a single thread waiting on a condition.
data Waiter =
  Waiter { waiterState    :: MVar WaiterState
         , waiterLock     :: MVar ()
         , waiterNotifier :: MVar (Maybe (MVar ()))
         }

-- | Condition data type.
data Condition =
  Condition { lock    :: MVar ()
            , waiters :: Chan Waiter }

-- | Creates a condition from a lock.
create :: MVar ()               -- ^ An 'MVar' to associate with condition.
       -> IO Condition
create lock = Condition lock
                    <$> newChan

-- | Creates a condition with hidden associated 'MVar' which can be accessed
-- using only 'acquire' and 'release' operations.
create' :: IO Condition
create' = newMVar () >>= create

-- | Acquires an underlying lock.
acquire :: Condition -> IO ()
acquire = takeMVar . lock

-- | Releases an underlying lock.
release :: Condition -> IO ()
release cond = putMVar (lock cond) ()

-- $lockStateInfo
-- All the following operations /must/ be called only when the lock associated
-- with condition is acquired. This can be done either by calling
-- 'Control.Concurrent.MVar.takeMVar' on the 'MVar' provided to 'create'
-- function or by calling 'acquire' on condition.

-- | Waits until notified.
-- Note that 'System.Timeout.timeout' can't be used safely with this function.
-- 'waitFor' /should/ be used instead.
wait :: Condition -> IO ()
wait (Condition lock waiters) = do
  checkLock lock "Control.Concurrent.Condition.wait"

  waiterState    <- newMVar Waiting
  waiterLock     <- newMVar ()
  waiterNotifier <- newEmptyMVar

  writeChan waiters (Waiter waiterState waiterLock waiterNotifier)

  putMVar lock ()

  _ <- takeMVar waiterNotifier
  barrier <- takeMVar waiterNotifier -- receiving notification

  -- if barrier has been returned we must wait on it
  unless (isNothing barrier) $ do
    takeMVar $ fromJust barrier

  _ <- takeMVar lock

  -- after acquiring a lock we can give other threads a possibility
  -- to pass a barrier
  unless (isNothing barrier) $ do
    putMVar (fromJust barrier) ()

  return ()

-- | Waits until notified or timeout run out.
waitFor :: Condition            -- ^ A condition to wait on.
        -> Int                  -- ^ Timeout in microseconds.
        -> IO Bool              -- ^ 'True' if notification has occured.
                                -- 'False' if timeout has passed.
waitFor (Condition lock waiters) time = do
  checkLock lock "Control.Concurrent.Condition.waitFor"

  waiterState    <- newMVar WaitingTimeout
  waiterLock     <- newMVar ()
  waiterNotifier <- newEmptyMVar

  writeChan waiters (Waiter waiterState waiterLock waiterNotifier)

  putMVar lock ()
  result <- timeout time (takeMVar waiterNotifier)

  (notified, barrier) <-
    case result of
      Nothing -> do               -- time ran out
        takeMVar waiterLock       -- accessing state atomically

        -- By this time notification may have been already received
        -- so this case must be checked explicitely. This can be done
        -- by checking the state of 'waiterNotifier' again.
        notified <- not <$> isEmptyMVar waiterNotifier

        -- if there are still no notifications changing the state to indicated
        -- that abort has been performed

        unless notified $
          modifyMVar_ waiterState (const $ return Aborted)

        barrier <- if notified then takeMVar waiterNotifier else return Nothing

        putMVar waiterLock ()

        return (notified, barrier)

      Just barrier ->
        return (True, barrier)      -- notification received; doing nothing


  unless (isNothing barrier) $ do
    takeMVar $ fromJust barrier

  _ <- takeMVar lock


  unless (isNothing barrier) $ do
    putMVar (fromJust barrier) ()

  return notified

-- | Helper notification function. Takes one waiter from the pool
-- and notifies it using barrier that provided.
notify' :: Condition            -- ^ A condition to notify on.
        -> Maybe (MVar ())      -- ^ A barrier.
        -> IO Bool              -- ^ 'True' some thread was notified.
                                -- 'False' no threads to notify left.
notify' (Condition lock waiters) barrier =
  fix $ \loop -> do
    waiter <- readChan waiters

    empty <- isEmptyChan waiters
    if empty
      then return False
      else do
        state <- readMVar $ waiterState waiter

        case state of
          Waiting        -> do
            -- no additional actions needed
            putMVar (waiterNotifier waiter) barrier
            return True
          WaitingTimeout -> do
            let lock = waiterLock waiter

            -- accessing waiter's state atomically
            -- NB: this lock can be released in two different places
            _ <- takeMVar lock

            -- the state may have changed by this time
            state <- readMVar $ waiterState waiter
            case state of
              Aborted        -> do
                putMVar lock ()
                loop     -- Can't do anything with this so 'wait' returns
                         -- as if time ran out.
              WaitingTimeout -> do
                -- notifying thread as usual
                putMVar (waiterNotifier waiter) barrier
                putMVar lock ()

                return True

          Aborted        -> loop


-- | Wakes up one of the threads waiting on the condition.
-- Does not release an associated lock.
notify :: Condition -> IO ()
notify condition = do
  checkLock (lock condition) "Control.Concurrent.Condition.notify"

  notify' condition Nothing
  return ()

-- | Wakes up all of the thread waiting on the condition.
-- Does not release an associated lock.
-- NB: At the time this function is unfair and may cause starvation.
notifyAll :: Condition -> IO ()
notifyAll condition = do
  checkLock (lock condition) "Control.Concurrent.Condition.notifyAll"

  barrier <- newEmptyMVar
  fix $ \loop -> do
    end <- notify' condition (Just barrier)

    unless end loop

  -- by this time all the threads are notified; releasing the barrier
  -- to allow to run one of them
  putMVar barrier ()

-- | Performs a check to ensure that a lock is acquired. If not then error
-- is issued. Used to check correctness of 'wait', 'notify' and 'notifyAll'
-- calls.
checkLock :: MVar a             -- ^ A lock to check.
          -> String             -- ^ Information added to the error message.
          -> IO ()
checkLock lock info = do
  empty <- isEmptyMVar lock
  unless empty $ error (info ++ " : lock is not acquired.")
