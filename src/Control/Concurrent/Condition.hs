------------------------------------------------------------------------------
-- |
-- Module      : Control.Concurrent.Condition
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Condition variables for Haskell built on top of 'Control.Concurrent.MVar'
-- and 'Control.Concurrent.Chan'.
--
-- This library is designed to be imported qualified. Recommended import
-- statements are the following:
--
-- > import Control.Concurrent.Condition (Condition)
-- > import qualified Control.Concurrent.Condition as Condition
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}


------------------------------------------------------------------------------
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

         -- * Utility functions.
       , with
       ) where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>) )
import Control.Concurrent.Chan ( Chan,
                                 newChan, writeChan, readChan, isEmptyChan )
import Control.Concurrent.MVar ( MVar,
                                 newMVar, readMVar, modifyMVar_ )
import Control.Exception ( bracket_ )
import Control.Monad ( when, unless )

import Data.Function ( fix )
import Data.Maybe ( isNothing, isJust, fromJust, fromMaybe )


import Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast

import Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock


------------------------------------------------------------------------------
-- | Possible states for 'Waiter'.
data WaiterState
  = Waiting                     -- ^ A waiter is waiting for notification
                                -- indefinetly long.
  | WaitingTimeout              -- ^ A waiter is waiting for notification
                                -- but can't be interrupted by timeout.
  | Aborted                     -- ^ A waiter has aborted its waiting due to
                                -- timeout.


------------------------------------------------------------------------------
-- | Represents a single thread waiting on a condition.
data Waiter =
  Waiter { waiterState    :: MVar WaiterState
         , waiterLock     :: Lock
         , waiterNotifier :: Broadcast (Maybe Lock)
         }


------------------------------------------------------------------------------
-- | Condition data type.
data Condition =
  Condition { lock    :: Lock
            , waiters :: Chan Waiter }


------------------------------------------------------------------------------
-- | Creates a condition from a lock.
create :: Lock               -- ^ An 'MVar' to associate with condition.
       -> IO Condition
create lock = Condition lock
                    <$> newChan


------------------------------------------------------------------------------
-- | Creates a condition with hidden associated 'MVar' which can be accessed
-- using only 'acquire' and 'release' operations.
create' :: IO Condition
create' = create =<< Lock.new


------------------------------------------------------------------------------
-- | Acquires an underlying lock.
acquire :: Condition -> IO ()
acquire = Lock.acquire . lock


------------------------------------------------------------------------------
-- | Releases an underlying lock.
release :: Condition -> IO ()
release (Condition lock _) = do
  checkLock lock "Control.Concurrent.Condition.release"

  Lock.release lock


------------------------------------------------------------------------------
-- $lockStateInfo
-- All the following operations /must/ be called only when the lock associated
-- with condition is acquired. This can be done either by calling
-- 'Control.Concurrent.MVar.takeMVar' on the 'MVar' provided to 'create'
-- function or by calling 'acquire' on condition.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Waits until notified.
-- Note that 'System.Timeout.timeout' can't be used safely with this function.
-- 'waitFor' /should/ be used instead.
wait :: Condition -> IO ()
wait (Condition { .. }) = do
  checkLock lock "Control.Concurrent.Condition.wait"

  waiterState    <- newMVar Waiting
  waiterLock     <- Lock.new
  waiterNotifier <- Broadcast.new

  writeChan waiters (Waiter waiterState waiterLock waiterNotifier)

  Lock.release lock

  barrier <- Broadcast.listen waiterNotifier

  -- if barrier has been returned we must wait on it
  unless (isNothing barrier) $
    Lock.acquire $ fromJust barrier

  Lock.acquire lock

  -- after acquiring a lock we can give other threads a possibility
  -- to pass a barrier
  unless (isNothing barrier) $
    Lock.release $ fromJust barrier

  return ()


------------------------------------------------------------------------------
-- | Waits until notified or timeout run out.
waitFor :: Condition            -- ^ A condition to wait on.
        -> Int                  -- ^ Timeout in microseconds.
        -> IO Bool              -- ^ 'True' if notification has occured.
                                -- 'False' if timeout has passed.
waitFor (Condition { .. }) time = do
  checkLock lock "Control.Concurrent.Condition.waitFor"

  waiterState    <- newMVar WaitingTimeout
  waiterLock     <- Lock.new
  waiterNotifier <- Broadcast.new

  writeChan waiters (Waiter waiterState waiterLock waiterNotifier)

  Lock.release lock

  -- TODO: Int <-> Integer
  result <- Broadcast.listenTimeout waiterNotifier (toInteger time)

  (notified, barrier) <-
    case result of
      Nothing -> do               -- time ran out
        Lock.acquire waiterLock   -- accessing state atomically

        -- By this time notification may have been already received
        -- so this case must be checked explicitely. This can be done
        -- by checking the state of 'waiterNotifier' again.
        maybeBarrier <- Broadcast.tryListen waiterNotifier
        let notified = isJust maybeBarrier

        -- if there are still no notifications then wer change the state to
        -- indicate that abort has been performed
        unless notified $
          modifyMVar_ waiterState (const $ return Aborted)

        let barrier = fromMaybe Nothing maybeBarrier

        Lock.release waiterLock

        return (notified, barrier)

      Just barrier ->
        return (True, barrier)      -- notification received; doing nothing


  unless (isNothing barrier) $
    Lock.acquire $ fromJust barrier

  Lock.acquire lock


  unless (isNothing barrier) $
    Lock.release $ fromJust barrier

  return notified


------------------------------------------------------------------------------
-- | Helper notification function. Takes one waiter from the pool
-- and notifies it using barrier that provided.
notify' :: Condition            -- ^ A condition to notify on.
        -> Maybe Lock           -- ^ A barrier.
        -> IO Bool              -- ^ 'True' some thread was notified.
                                -- 'False' no threads to notify left.
notify' (Condition { .. }) barrier =
  fix $ \loop -> do
    empty <- isEmptyChan waiters
    if empty
      then return False
      else do
        waiter <- readChan waiters
        state  <- readMVar $ waiterState waiter

        case state of
          Waiting        -> do
            -- no additional actions needed
            Broadcast.signal (waiterNotifier waiter) barrier
            return True
          WaitingTimeout -> do
            let lock = waiterLock waiter

            -- accessing waiter's state atomically
            -- NB: this lock can be released in two different places
            Lock.acquire lock

            -- the state may have changed by this time
            state <- readMVar $ waiterState waiter
            case state of
              Aborted        -> do
                Lock.release lock
                loop     -- Can't do anything with this so 'wait' returns
                         -- as if time ran out.
              WaitingTimeout -> do
                -- notifying thread as usual
                Broadcast.signal (waiterNotifier waiter) barrier
                Lock.release lock

                return True
              _              -> error "notify': impossible happened"

          Aborted        -> loop


------------------------------------------------------------------------------
-- | Wakes up one of the threads waiting on the condition.
-- Does not release an associated lock.
notify :: Condition -> IO ()
notify condition = do
  checkLock (lock condition) "Control.Concurrent.Condition.notify"

  notify' condition Nothing
  return ()


------------------------------------------------------------------------------
-- | Wakes up all of the thread waiting on the condition.
-- Does not release an associated lock.
-- NB: At the time this function is unfair and may cause starvation.
notifyAll :: Condition -> IO ()
notifyAll condition = do
  checkLock (lock condition) "Control.Concurrent.Condition.notifyAll"

  barrier <- Lock.newAcquired
  fix $ \loop -> do
    notified <- notify' condition (Just barrier)

    when notified loop

  -- by this time all the threads are notified; releasing the barrier
  -- to allow to run one of them
  Lock.release barrier


------------------------------------------------------------------------------
-- | Performs a check to ensure that a lock is acquired. If not then error
-- is issued. Used to check correctness of 'wait', 'notify' and 'notifyAll'
-- calls.
checkLock :: Lock               -- ^ A lock to check.
          -> String             -- ^ Information added to the error message.
          -> IO ()
checkLock lock info = do
  locked <- Lock.locked lock

  unless locked $ error (info ++ " : lock is not acquired.")


------------------------------------------------------------------------------
-- | Acquires a condition, executes an action, releases condition.
-- Exception safe.
with :: Condition               -- ^ A condtion to lock on.
     -> IO a                    -- ^ Action to execute.
     -> IO a
with cond = bracket_ (acquire cond)
                     (release cond)
