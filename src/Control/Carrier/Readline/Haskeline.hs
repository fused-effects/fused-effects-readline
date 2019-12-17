{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline carrier
  runReadline
, runReadlineWithHistory
, ReadlineC(..)
  -- * Readline effect
, module Control.Effect.Readline
) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Readline
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Coerce (coerce)
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.FilePath

runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings (ReadlineC m) = runInputTWithPrefs prefs (coerce settings) (runM (runReader (Line 0) m))

runReadlineWithHistory :: MonadException m => ReadlineC m a -> m a
runReadlineWithHistory block = do
  homeDir <- liftIO getHomeDirectory
  prefs   <- liftIO $ readPrefs (homeDir </> ".haskeline")
  prog    <- liftIO getExecutablePath
  let settingsDir = homeDir </> ".local" </> dropExtension (takeFileName prog)
      settings = Settings
        { complete = noCompletion
        , historyFile = Just (settingsDir </> "repl_history")
        , autoAddHistory = True
        }
  liftIO $ createDirectoryIfMissing True settingsDir

  runReadline prefs settings block

newtype ReadlineC m a = ReadlineC (ReaderC Line (LiftC (InputT m)) a)
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadTrans ReadlineC where
  lift = ReadlineC . lift . lift . lift


newtype Line = Line Int
