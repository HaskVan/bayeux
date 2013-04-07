{-# LANGUAGE OverloadedStrings #-}
module TestLoop.Main where

--------------------

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (forM_, forever)
import           Data.Monoid               (mconcat)
import           System.FilePath           (joinPath, takeExtension)
import           System.IO                 (hPutStrLn, stderr)

--------------------

import qualified Filesystem.Path.CurrentOS as FS

--------------------

import           System.FSNotify           (watchTree, withManager)
import           System.FSNotify.Devel     (doAllEvents, existsEvents,
                                            treeExtExists)
--------------------

import           TestLoop.Internal.Cabal
import           TestLoop.Internal.Types
import           TestLoop.Internal.Watcher
import           TestLoop.Util

--------------------------------------------------------------------------------

startTestLoop :: MainModuleName -> MainModulePath -> HsSourcePaths -> IO ()
startTestLoop moduleName modulePath paths =
   withManager $ \manager -> do
     forM_ paths $ \path -> do
       treeExtExists manager
                     (FS.decodeString path)
                     "hs"
                     (reloadTestSuite moduleName modulePath paths)
     forever $ threadDelay 100

--------------------------------------------------------------------------------

setupTestLoop :: IO ()
setupTestLoop = do
 (testsuite, moduleFile, sourcePaths) <- parseCabalFile
 if not ("test" `elem` sourcePaths)
    then hPutStrLn stderr (mconcat [ "You must have a `test` folder in "
                                   , "your cabal's test-suite hs-source-paths"])
    else do
      putStrLn $ "Test Loop starting on test-suite " ++ testsuite
      putStrLn $ "Listening files on source paths: " ++ (join " " sourcePaths)
      forkIO $ startTestLoop "Main"
                             (joinPath ["test", moduleFile])
                             sourcePaths
      forever $ threadDelay 100
