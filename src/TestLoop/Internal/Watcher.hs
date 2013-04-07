module TestLoop.Internal.Watcher where

--------------------------------------------------------------------------------


import           Control.Monad.Trans                 (MonadIO (..))
import qualified Filesystem.Path                     as FS
import qualified Filesystem.Path.CurrentOS           as FS

--------------------

import           Language.Haskell.Interpreter        (as, interpret,
                                                      loadModules, setImportsQ,
                                                      setTopLevelModules)
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)

--------------------

import TestLoop.Util
import           TestLoop.Internal.Types

--------------------------------------------------------------------------------

reloadTestSuite :: MainModuleName
                -> MainModulePath
                -> HsSourcePaths
                -> FS.FilePath
                -> IO ()
reloadTestSuite moduleName modulePath sourcePaths modifiedFile
  | isNotEmacsFile = reloadTestSuite_
  | otherwise = return ()
  where
    isNotEmacsFile = not ('#' `elem` (FS.encodeString $ FS.filename modifiedFile))
    reloadTestSuite_ = do
      putStrLn $ "File Changed: " ++ (FS.encodeString  modifiedFile)
      r <- runInterpreter
      case r of
        Right () -> return ()
        Left e   -> print e

    runInterpreter = unsafeRunInterpreterWithArgs
                       [ "-i " ++ join ":" sourcePaths
                       , "-package-conf cabal-dev/packages-7.6.1.conf"]
                       interpreterAction

    interpreterAction = do
      loadModules [modulePath]
      setTopLevelModules [moduleName]
      setImportsQ [("Prelude", Nothing)]
      execution <- interpret "main" (as :: IO ())
      liftIO $ execution
