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
-- Eating philosophers using semaphors implemented on top of
-- 'Control.Concurrent.Condition'.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module Main
       (
         main
       ) where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad ( when, forever, mapM )

import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Data.Time ( getCurrentTime, diffUTCTime )

import System.Random ( randomRIO )

------------------------------------------------------------------------------
import Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock

import Control.Concurrent.Condition ( Condition, with )
import qualified Control.Concurrent.Condition as Condition


------------------------------------------------------------------------------
--              Semaphores implemented on top of conditions.                --
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
-- | Creates a semaphore with specific initial value.
new_ :: Int -> IO Semaphore
new_ value = Semaphore <$> newIORef value
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


------------------------------------------------------------------------------


------------------------------------------------------------------------------
--                            Eating philosophers.                          --
------------------------------------------------------------------------------
-- | Maximum time in seconds philosopher can spend on thinking.
maxThinkingTime :: Int
maxThinkingTime = 20


------------------------------------------------------------------------------
-- | Maximum time in seconds philosopher can spend on eating. 
maxEatingTime :: Int
maxEatingTime = 10


------------------------------------------------------------------------------
-- | Number of philosophers to simulate.
philosophersCount :: Int
philosophersCount = 10


------------------------------------------------------------------------------
-- | Delay current thread for the specified number of seconds.
sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)


------------------------------------------------------------------------------
-- | Type for function to say something.
type Say = String -> IO ()


------------------------------------------------------------------------------
-- | Simulates philosopher's thinking.
think :: Say -> IO ()
think say = do
  time <- randomRIO (1, maxThinkingTime)

  say $ "It's time to think for " ++ show time ++ " seconds."
  sleep time
  say "Enough thinking for now."


------------------------------------------------------------------------------
-- | Fork is a pair of its identifier and a semaphore.
data Fork = Fork Int Semaphore


------------------------------------------------------------------------------
instance Show Fork where
  show (Fork id _) = "Fork: id=" ++ show id


------------------------------------------------------------------------------
-- | Creates a fork with the specified identifier.
mkFork :: Int -> IO Fork
mkFork id = Fork id
             <$> new_ 1


------------------------------------------------------------------------------
-- | Acquires a fork.
acquire :: Fork -> IO ()
acquire (Fork _ sem) = wait sem


------------------------------------------------------------------------------
-- | Releases a fork.
release :: Fork -> IO ()
release (Fork _ sem) = post sem


------------------------------------------------------------------------------
eat :: Say -> (Fork, Fork) -> IO ()
eat say (left, right) = do
  time <- randomRIO (1, maxEatingTime)

  say $ "It's time to eat for " ++ show time ++ " seconds."

  say $ "Acquiring left fork (" ++ show left ++ ")."
  acquire left
  say $ "Left fork acquired."

  say $ "Acquiring right fork (" ++ show left ++ ")."
  acquire right
  say $ "Right fork acquired."  

  say "Can begin eating now."
  sleep time
  say "Done with eating for now."

  say $ "Releasing left fork (" ++ show left ++ ")."
  release left
  say $ "Left fork released."

  say $ "Releasing right fork (" ++ show left ++ ")."
  release right
  say $ "Right fork released."  
  


------------------------------------------------------------------------------
-- | Simulates single philosopher.
philosopher :: Say -> (Fork, Fork) -> IO ()
philosopher say forks = do
  forkIO $
    forever $ do
      think say
      eat say forks

  return ()


------------------------------------------------------------------------------  
main :: IO ()
main = do
  forks <- mapM mkFork [0 .. philosophersCount]
  says  <- mapM mkSay  [0 .. philosophersCount]

  let forks' = zip forks (tail forks ++ [head forks])
  let phs    = philosopher <$> says <*> forks'

  sequence_ phs

  sleep 60

  where mkSay :: Int -> IO Say
        mkSay num = do
          lock  <- Lock.new
          start <- getCurrentTime

          let say msg =
                Lock.with lock $ do
                  time <- getCurrentTime
                  let diff = diffUTCTime time start

                  putStrLn $ show diff ++ " : philosopher "
                                       ++ show num ++ " : " ++ msg

          return say
