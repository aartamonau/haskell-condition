------------------------------------------------------------------------------
-- |
-- Module      : Control.Concurrent.Condition.Test
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Unit tests for 'Control.Concurrent.Condiion' module. Mainly copied from
-- python's tests of threading.Condition.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module Control.Concurrent.Condition.Test ( tests )
       where


------------------------------------------------------------------------------
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.MVar ( MVar,
                                 newMVar, newEmptyMVar,
                                 takeMVar, putMVar, readMVar, modifyMVar_,
                                 withMVar )
import Control.Monad ( replicateM, replicateM_, when )

import Data.Function ( fix )
import Data.Time ( getCurrentTime, diffUTCTime )


------------------------------------------------------------------------------
import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( Assertion, assertEqual, assertBool )


------------------------------------------------------------------------------
import Control.Concurrent.Condition ( Condition, with )
import qualified Control.Concurrent.Condition as Condition


------------------------------------------------------------------------------
import TestUtils ( assertThrows, assertBlocking', assertNonBlocking' )


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCase "acquire/release"    test_acquire
        , testCase "with/release"       test_with
        , testCase "unacquired wait"    test_unacquiredWait
        , testCase "unacquired notify"  test_unacquiredNotify
        , testCase "unacquired release" test_unacquiredRelease
        , testCase "notify"             test_notify
        , testCase "timeout"            test_timeout
        ]


------------------------------------------------------------------------------
test_acquire :: Assertion
test_acquire = do
  cond <- Condition.new_

  assertNonBlocking' "'acquire' blocks on unacquired condition" $
    Condition.acquire cond
  assertBlocking' "'acquire' does not block on acquired condition" $
    Condition.acquire cond

  assertNonBlocking' "'release' of acquired condition blocks" $
    Condition.release cond
  assertNonBlocking' "'acquire' of condition after acquire/release blocks" $
    Condition.acquire cond


------------------------------------------------------------------------------
test_with :: Assertion
test_with = do
  cond <- Condition.new_

  with cond $
    assertBlocking' "'acquire' within 'with' does not block" $
      Condition.acquire cond

  assertNonBlocking' "'acquire' blocks after 'with'" $
    Condition.acquire cond


------------------------------------------------------------------------------
test_unacquiredWait :: Assertion
test_unacquiredWait = do
  cond <- Condition.new_

  assertThrows "'wait' keeps silence on unacquired condition" $
    Condition.wait cond
  assertThrows "'waitFor' keeps silence on unacquired condition" $
    Condition.waitFor cond 123456


------------------------------------------------------------------------------
test_unacquiredNotify :: Assertion
test_unacquiredNotify = do
  cond <- Condition.new_

  assertThrows "'notify' keeps silence on unacquired condition" $
    Condition.notify cond
  assertThrows "'notifyAll' keeps silence on unacquired condition" $
    Condition.notifyAll cond


------------------------------------------------------------------------------
test_unacquiredRelease :: Assertion
test_unacquiredRelease = do
  cond <- Condition.new_

  assertThrows "'release' keeps silence on unacquired condition" $
    Condition.release cond


------------------------------------------------------------------------------
checkNotify :: Condition -> Assertion
checkNotify cond = do
  results1 <- newMVar []
  results2 <- newMVar []

  phase    <- newMVar 0 :: IO (MVar Int)

  let f flag = do
        with cond $ do
          Condition.wait cond
          modifyMVar_ results1 appendPhase

        with cond $ do
          Condition.wait cond
          modifyMVar_ results2 appendPhase

        putMVar flag ()

      appendPhase ps = readMVar phase >>= \p -> return (p : ps)

  flags <- replicateM 5 newEmptyMVar
  mapM_ (forkIO . f) flags

  wait

  readMVar results1 >>= assertEqual "incorrect results1 (phase 0)" []
  readMVar results2 >>= assertEqual "incorrect results2 (phase 0)" []

  with cond $ do
    replicateM_ 3 (Condition.notify cond)
    modifyMVar_ phase (return . (+1))

  fix $ \loop -> do
    n <- withMVar results1 (return . length)
    when (n < 3) $ wait >> loop

  readMVar results1 >>= assertEqual "incorrect results1 (phase 1)" [1, 1, 1]
  readMVar results2 >>= assertEqual "incorrect results2 (phase 1)" []

  wait

  with cond $ do
    Condition.notifyAll cond
    modifyMVar_ phase (return . (+1))

  fix $ \loop -> do
    n1 <- withMVar results1 (return . length)
    n2 <- withMVar results2 (return . length)

    when (n1 + n2 < 8) $ wait >> loop

  readMVar results1 >>=
    assertEqual "incorrect results1 (phase 2)" [2, 2, 1, 1, 1]
  readMVar results2 >>= assertEqual "incorrect results2 (phase 2)" [2, 2, 2]

  wait

  with cond $ do
    Condition.notifyAll cond
    modifyMVar_ phase (return . (+1))

  fix $ \loop -> do
    n <- withMVar results2 (return . length)
    when (n < 5) $ wait >> loop

  readMVar results1 >>=
    assertEqual "incorrect results1 (phase 3)" [2, 2, 1, 1, 1]
  readMVar results2 >>=
    assertEqual "incorrect results2 (phase 3)" [3, 3, 2, 2, 2]

  -- wait till all the threads finish
  mapM_ takeMVar flags

  where wait = threadDelay 500000


------------------------------------------------------------------------------
test_notify :: Assertion
test_notify = do
  cond <- Condition.new_

  checkNotify cond
  checkNotify cond


------------------------------------------------------------------------------
test_timeout :: Assertion
test_timeout = do
  cond    <- Condition.new_
  results <- newMVar []

  let f flag = do
        with cond $ do
          t1 <- getCurrentTime

          Condition.waitFor cond 500000

          t2 <- getCurrentTime

          let diff = fromEnum (diffUTCTime t2 t1) `div` 1000000

          modifyMVar_ results (return . (diff:))

        putMVar flag ()

  flags <- replicateM 5 newEmptyMVar
  mapM (forkIO . f) flags

  mapM_ takeMVar flags

  withMVar results $ \rs -> do
    assertEqual "invalid length of results" (length rs) 5

    mapM_ (assertBool "to little time passed" . (>= 500000)) rs
