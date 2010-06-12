#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
module Main ( main )
       where


------------------------------------------------------------------------------
import Distribution.PackageDescription ( PackageDescription )
import Distribution.Simple ( Args, UserHooks ( runTests ),
                             defaultMainWithHooks, simpleUserHooks )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo )

import System.Cmd ( system )
import System.FilePath ( (</>) )


------------------------------------------------------------------------------
main :: IO()
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests }

        runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
        runTests _ _ _ _ = system cmd >> return ()
          where cmd = "." </> "dist" </> "build" </> "test" </> "test"
