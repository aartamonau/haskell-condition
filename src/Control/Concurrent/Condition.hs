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

import Control.Concurrent.MVar (MVar)

-- | Condition data type.
data Condition = Condition

-- | Creates a condition from a lock.
create :: MVar ()                -- ^ An 'MVar' to associate with condition.
       -> IO Condition
create = undefined

-- | Creates a condition with hidden associated 'MVar' which can be accessed
-- using only 'acquire' and 'release' operations.
create' :: IO Condition
create' = undefined

-- | Acquire an underlying lock.
acquire :: Condition -> IO ()
acquire = undefined

-- | Release an underlying lock.
release :: Condition -> IO ()
release = undefined

-- $lockStateInfo
-- All the following operations /must/ be called only when the lock associated
-- with condition is acquired. This can be done either by calling
-- 'Control.Concurrent.MVar.takeMVar' on the 'MVar' provided to 'create' function
-- or by calling 'acquire' on condition.

-- | Wait until notified.
wait :: Condition -> IO ()
wait = undefined

-- | Wake up one of the threads waiting on the condition.
notify :: Condition -> IO ()
notify = undefined

-- | Wake up all of the thread waiting on the condition.
notifyAll :: Condition -> IO ()
notifyAll = undefined
