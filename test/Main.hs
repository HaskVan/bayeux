{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative                         ((<$>), (<*>))
import           Control.Concurrent                          (MVar, forkIO,
                                                              killThread,
                                                              modifyMVar_,
                                                              myThreadId,
                                                              newMVar, readMVar,
                                                              threadDelay)
import           Control.Exception                           (AsyncException,
                                                              SomeException,
                                                              catch)
import           Control.Monad                               (forever, when)
import           Control.Monad.Trans                         (liftIO)
import           Data.Monoid                                 (First (..),
                                                              mconcat)
import           Data.String                                 (IsString (..))
import qualified Filesystem.Path                             as FS
import           Language.Haskell.Interpreter
import           Language.Haskell.Interpreter.Unsafe         (unsafeRunInterpreterWithArgs)
import           System.Directory                            (getCurrentDirectory)
import           System.Environment                          (getArgs)
import           System.Exit                                 (exitFailure)
import           System.FSNotify                             (watchTree,
                                                              withManager)
import           System.FSNotify.Devel                       (doAllEvents,
                                                              existsEvents,
                                                              treeExtExists)
import           System.Posix.Signals                        (Handler (Catch),
                                                              installHandler,
                                                              sigINT)

--------------------------------------------------------------------------------
import           Distribution.PackageDescription             (CondTree (..), GenericPackageDescription (..), PackageDescription (..),
                                                              TestSuite (..), TestSuiteInterface (..),
                                                              condTreeData,
                                                              hsSourceDirs,
                                                              testBuildInfo)
import           Distribution.PackageDescription.Parse       (readPackageDescription)
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import           Distribution.Verbosity                      (normal)


--------------------------------------------------------------------------------

-- reloadTestSuite :: FS.FilePath -> IO ()
-- reloadTestSuite modifiedFile
--   | isNotEmacsFile = reloadTestSuite_
--   | otherwise = return ()
--   where
--     isNotEmacsFile = not ('#' `elem` (show $ FS.filename modifiedFile))
--     reloadTestSuite_ = do
--       putStrLn $ "File Changed: " ++ (show modifiedFile)
--       r <- runInterpreter
--       case r of
--         Right () -> return ()
--         Left e   -> print e

--     runInterpreter = unsafeRunInterpreterWithArgs
--                        [ "-i src:test"
--                        , "-package-conf cabal-dev/packages-7.6.1.conf"]
--                        interpreterAction

--     interpreterAction = do
--       loadModules ["test/TestSuite.hs"]
--       setTopLevelModules ["TestSuite"]
--       setImportsQ [("Prelude", Nothing)]
--       execution <- interpret "runTests" (as :: IO ())
--       liftIO $ execution

-- exitSuccess' :: MVar Bool -> SomeException -> IO ()
-- exitSuccess' running _ = modifyMVar_ running (const $ return False)

--------------------------------------------------------------------------------

-- type TestSuiteName = String
-- type MainModuleName = String
-- type HsSourcePaths = [String]

-- getTestSuiteToRun :: IO (Maybe String)
-- getTestSuiteToRun = do
--     args <- getArgs
--     case args of
--       (x:_) -> return (Just x)
--       _ -> return Nothing

-- parseTestSuiteInfo :: Maybe String
--                    -> (String, CondTree a b TestSuite)
--                    -> Maybe (String, String, [String])
-- parseTestSuiteInfo (Just inputName) (name, CondNode { condTreeData=testSuite })
--     | inputName == name =
--       case testInterface testSuite of
--         TestSuiteExeV10 _ file -> Just (name, file, hsSourceDirs $ testBuildInfo testSuite)
--         _ -> Nothing
--     | otherwise = Nothing
-- parseTestSuiteInfo Nothing input@(name, _) = parseTestSuiteInfo (Just name) input

-- getCabalFilePath :: IO FilePath
-- getCabalFilePath = return "bayeux.cabal"

-- parseCabalFile_ :: Maybe String
--                 -> GenericPackageDescription
--                 -> Maybe (String, String, [String])
-- parseCabalFile_ testSuiteName genericPackDesc =
--     getFirst . mconcat $ map (First . parseTestSuiteInfo testSuiteName)
--                              (condTestSuites genericPackDesc)

-- parseCabalFile :: IO (TestSuiteName, MainModuleName, HsSourcePaths)
-- parseCabalFile = do
--     cabalFilePath <- getCabalFilePath
--     result <- parseCabalFile_ <$> getTestSuiteToRun
--                               <*> readPackageDescription normal cabalFilePath
--     maybe (error $ msg ++ cabalFilePath)
--           return
--           result
--   where
--     msg = mconcat [ "You need to have at least one test-suite "
--                   , "with type == exitcode-stdio-1.0 on "]


--------------------------------------------------------------------------------

main :: IO ()
main = do
  parseCabalFile >>= print

--------------------------------------------------------------------------------

runTestSuiteOnChange :: MVar Bool -> IO ()
runTestSuiteOnChange running = do
    withManager $ \manager -> do
      dir <- getCurrentDirectory
      putStrLn $ "Listening directory: " ++ dir
      treeExtExists manager (fromString dir) "hs" reloadTestSuite
      forever $ do
        val <- readMVar running
        if val
          then threadDelay 100
          else myThreadId >>= killThread

--------------------------------------------------------------------------------
