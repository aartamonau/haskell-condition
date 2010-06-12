module Main ( main )
       where


------------------------------------------------------------------------------
import Test.Framework ( defaultMain )


------------------------------------------------------------------------------
import Control.Concurrent.Condition.Test ( tests )


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
