------------------------------------------------------------------------------
-- |
-- Module      : BarberShop
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Sleeping barber shop problem using 'Control.Concurrent.Condition'.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}


------------------------------------------------------------------------------
module Main ( main )
       where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( unless )

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar, newMVar, withMVar )

import Data.Function ( fix )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import System.Random ( randomRIO )


------------------------------------------------------------------------------
import Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock


import Control.Concurrent.Condition ( Condition )
import qualified Control.Concurrent.Condition as Condition


------------------------------------------------------------------------------
-- | Barber shop description.
data BarberShop =
  BarberShop { lock                :: Lock       -- ^ A lock associated with all
                                                 -- conditions.

             , barberAvailable     :: IORef Bool -- ^ Is barber currently free?
             , barberAvailableCond :: Condition

             , chairOccupied       :: IORef Bool -- ^ Is the currently occupied?
             , chairOccupiedCond   :: Condition

             , doorOpen            :: IORef Bool -- ^ Is the exit door open?
             , doorOpenCond        :: Condition

             , customerLeftCond    :: Condition  -- ^ Used to notify the barber
                                                 -- that some customer's left
                                                 -- the barber shop.
             }


------------------------------------------------------------------------------
-- | A type of functions that allow barber and customers to say something.
type Say = String -> IO ()


------------------------------------------------------------------------------
-- | Creates and initializes a barber shop.
initBS :: IO BarberShop
initBS = do
  lock <- Lock.new

  BarberShop lock
         <$> newIORef True
         <*> Condition.new lock

         <*> newIORef False
         <*> Condition.new lock

         <*> newIORef False
         <*> Condition.new lock

         <*> Condition.new lock


------------------------------------------------------------------------------
-- | A process of getting a haircut from a custmer's point of view.
getHaircut :: Say -> BarberShop -> IO ()
getHaircut say (BarberShop { .. }) =
  Lock.with lock $ do
    say "checking whether barber is available"
    available <- readIORef barberAvailable
    unless available $ do
      say "waiting until barber gets free"
      Condition.wait barberAvailableCond
    writeIORef barberAvailable False

    say "taking the chair"
    writeIORef chairOccupied True
    Condition.notify chairOccupiedCond

    say "checking whether door is open"
    open <- readIORef doorOpen
    unless open $ do
      say "waiting until door is open"
      Condition.wait doorOpenCond

    say "closing the door"
    writeIORef doorOpen False
    Condition.notify customerLeftCond


------------------------------------------------------------------------------
-- | A process finding a next customer from barber's point of view.
getNextCustomer :: Say -> BarberShop -> IO ()
getNextCustomer say (BarberShop { .. }) =
  Lock.with lock $ do
    writeIORef barberAvailable True
    Condition.notify barberAvailableCond

    say "checking whether some customer is in the chair"
    occupied <- readIORef chairOccupied
    unless occupied $ do
      say "waiting until someone occupies the chair"
      Condition.wait chairOccupiedCond


------------------------------------------------------------------------------
-- | A barber's protocol of behavior after some customer has been served.
finishHaircut :: Say -> BarberShop -> IO ()
finishHaircut say (BarberShop { .. }) =
  Lock.with lock $ do
    say "opening the door"
    writeIORef doorOpen True
    Condition.notify doorOpenCond

    say "waiting till the customer leaves"
    Condition.wait customerLeftCond


------------------------------------------------------------------------------
-- | Maximum delay in seconds between two incoming customers.
maxCustomerDelay :: Int
maxCustomerDelay = 20


------------------------------------------------------------------------------
-- | Average duration of haircut.
haircutDuration :: Int
haircutDuration = 10


------------------------------------------------------------------------------
-- | Maximum absolute haircut duration deflection from the average one.
haircutVariation :: Int
haircutVariation = 3


------------------------------------------------------------------------------
-- | Delay the thread on the specified number of seconds.
wait :: Int -> IO ()
wait = threadDelay . (* 1000000)


------------------------------------------------------------------------------
-- | Barbers behavior.
barber :: Say -> BarberShop -> IO ()
barber say bs = do
  say "is there anybody wanting to get new haircut?"
  getNextCustomer say bs

  say "cutting in progress"
  duration <- getHaircutDuration
  wait duration
  say "finished; good work"

  finishHaircut say bs
  say "bye"

  barber say bs

  where getHaircutDuration :: IO Int
        getHaircutDuration = do
          variation <- randomRIO (-haircutVariation, haircutVariation)

          return $ haircutDuration + variation


------------------------------------------------------------------------------
-- | Every single customer's behavior.
customer :: Say -> BarberShop -> IO ()
customer say bs = do
  say "on my way to barber shop"
  getHaircut say bs
  say "see you"


------------------------------------------------------------------------------
main :: IO ()
main = do
  bs   <- initBS

  sayLock <- newMVar ()

  _ <- forkIO (barber (mkSay sayLock "Barber") bs)

  flip fix 0 $ \loop id -> do
    delay <- randomRIO (0, maxCustomerDelay)
    wait delay
    _ <- forkIO (customer (mkSay sayLock ("Customer-" ++ show id)) bs)

    loop (id + 1)

  return ()

  where mkSay :: MVar () -> String -> Say
        mkSay lock id msg =
          withMVar lock $ const $ putStrLn (id ++ ": " ++ msg)
