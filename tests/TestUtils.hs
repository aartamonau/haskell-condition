------------------------------------------------------------------------------
-- |
-- Module      : TestUtils
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Utility functions used in unit tests.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module TestUtils ( assertThrows,
                   assertBlocking, assertBlocking',
                   assertNonBlocking, assertNonBlocking' )
       where


------------------------------------------------------------------------------
import Control.Exception ( onException )
import System.Timeout ( timeout )

------------------------------------------------------------------------------
import Test.HUnit ( Assertion, assertFailure )


------------------------------------------------------------------------------
-- | Asserts that a computation raises some exception.
assertThrows :: String          -- ^ Failure message.
             -> IO a            -- ^ The computation.
             -> Assertion
assertThrows msg c =
  (c >> return ()) `onException` assertFailure msg


------------------------------------------------------------------------------
-- | Asserts that a computation blocks. This is supposed to be true if the
-- computation does not finish within given time.
assertBlocking :: Int             -- ^ Time period in microseconds.
               -> String          -- ^ Failure message.
               -> IO a            -- ^ The computation. Must handle asynchronous
                                  -- exceptions correctly.
               -> Assertion
assertBlocking t msg c = do
  r <- timeout t c
  case r of
    Nothing -> return ()
    Just _  -> assertFailure msg


------------------------------------------------------------------------------
-- | Asserts that a computation does not block within a given period of time.
assertNonBlocking :: Int        -- ^ Time period in microseconds.
                  -> String     -- ^ Failure message.
                  -> IO a       -- ^ The computation. Must handle asynchronous
                                -- exceptions correctly.
                  -> Assertion
assertNonBlocking t msg c = assertThrows msg (assertBlocking t "" c)


------------------------------------------------------------------------------
-- | Default blocking period used by 'assertBlocking\'' and
-- 'assertNonBlocking\''.
defaultBlockingPeriod :: Int
defaultBlockingPeriod = 1000000


------------------------------------------------------------------------------
-- | Like 'assertBlocking' with default blocking period of 1000000
-- microseconds.
assertBlocking' :: String
                -> IO a
                -> Assertion
assertBlocking' = assertBlocking defaultBlockingPeriod


------------------------------------------------------------------------------
-- | Like 'assertNonBlocking' with default blocking period of 1000000
-- microseconds.
assertNonBlocking' :: String
                   -> IO a
                   -> Assertion
assertNonBlocking' = assertNonBlocking defaultBlockingPeriod
