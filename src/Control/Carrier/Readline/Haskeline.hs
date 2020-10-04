{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline carrier
  runReadline
, runReadlineWithHistory
, ReadlineC(ReadlineC)
  -- * Readline effect
, module Control.Effect.Readline
) where

import Control.Algebra
import Control.Effect.Readline
#if MIN_VERSION_haskeline(0, 8, 0)
import Control.Monad.Catch (MonadMask(..))
#endif
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Console.Haskeline as H
import System.Directory
import System.Environment
import System.FilePath

#if MIN_VERSION_haskeline(0, 8, 0)
runReadline :: (MonadIO m, MonadMask m) => Prefs -> Settings m -> ReadlineC m a -> m a
#else
runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
#endif
runReadline prefs settings (ReadlineC m) = runInputTWithPrefs prefs settings m

#if MIN_VERSION_haskeline(0, 8, 0)
runReadlineWithHistory :: (MonadIO m, MonadMask m) => ReadlineC m a -> m a
#else
runReadlineWithHistory :: MonadException m => ReadlineC m a -> m a
#endif
runReadlineWithHistory block = do
  (prefs, settings) <- liftIO $ do
    homeDir <- getHomeDirectory
    prefs   <- readPrefs (homeDir </> ".haskeline")
    prog    <- getExecutablePath
    let settingsDir = homeDir </> ".local" </> dropExtension (takeFileName prog)
        settings = Settings
          { complete       = noCompletion
          , historyFile    = Just (settingsDir </> "repl_history")
          , autoAddHistory = True
          }
    createDirectoryIfMissing True settingsDir
    pure (prefs, settings)

  runReadline prefs settings block

newtype ReadlineC m a = ReadlineC { runReadlineC :: InputT m a }
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO, MonadTrans)

#if MIN_VERSION_haskeline(0, 8, 0)
instance (MonadIO m, MonadMask m) => Algebra Readline (ReadlineC m) where
#else
instance MonadException m => Algebra Readline (ReadlineC m) where
#endif
  alg hdl sig ctx = case sig of
    GetInputLine prompt -> (<$ ctx) <$> ReadlineC (H.getInputLine prompt)
    GetInputLineWithInitial prompt lr -> (<$ ctx) <$> ReadlineC (H.getInputLineWithInitial prompt lr)
    GetInputChar prompt -> (<$ ctx) <$> ReadlineC (H.getInputChar prompt)
    GetPassword c prompt -> (<$ ctx) <$> ReadlineC (H.getPassword c prompt)
    WaitForAnyKey prompt -> (<$ ctx) <$> ReadlineC (H.waitForAnyKey prompt)
    OutputStr s -> (<$ ctx) <$> ReadlineC (H.outputStr s)
    WithInterrupt m -> ReadlineC (H.withInterrupt (runReadlineC (hdl (m <$ ctx))))
