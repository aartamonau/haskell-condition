------------------------------------------------------------------------------
-- |
-- Module      : Semaphore
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Semaphore implementation using 'Control.Concurrent.Condition'.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module Semaphore
       (
         -- * @Semaphore@ type.
         Semaphore

         -- * Creation.
       , new

         -- * Operations.
       , post
       , wait
       ) where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( when )

import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )


------------------------------------------------------------------------------
import Control.Concurrent.Condition ( Condition, with )
import qualified Control.Concurrent.Condition as Condition


------------------------------------------------------------------------------
-- | Type representation of semaphore.
data Semaphore =
  Semaphore { value :: IORef Int -- ^ Current value of semaphore.
            , cond  :: Condition -- ^ Condition used to synchronize access.
            }


------------------------------------------------------------------------------
-- | Creates a semaphore with initial value of zero.
new :: IO Semaphore
new = Semaphore <$> newIORef 0
                <*> Condition.new_


------------------------------------------------------------------------------
-- | Increases a value of semaphore by 1.
post :: Semaphore -> IO ()
post (Semaphore value cond) =
  with cond $ do
    modifyIORef value (+1)

    Condition.notify cond


------------------------------------------------------------------------------
-- | Decreases a value of semaphore by 1. Blocks if the value is equal to
-- zero.
wait :: Semaphore -> IO ()
wait (Semaphore value cond) =
  with cond $ do
    empty <- fmap (== 0) (readIORef value)
    when empty (Condition.wait cond)

    modifyIORef value (flip (-) 1)
