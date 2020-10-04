{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Effect.Readline
#if MIN_VERSION_haskeline(0, 8, 0)
import Control.Monad.Catch (MonadMask(..))
#endif
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Console.Haskeline
import System.Console.Terminal.Size as Size
import System.Directory
import System.Environment
import System.FilePath
import System.IO (stdout)

#if MIN_VERSION_haskeline(0, 8, 0)
runReadline :: (MonadIO m, MonadMask m) => Prefs -> Settings m -> ReadlineC m a -> m a
#else
runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
#endif
runReadline prefs settings (ReadlineC m) = runInputTWithPrefs prefs settings (snd <$> m 0)

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

newtype ReadlineC m a = ReadlineC (Int -> InputT m (Int, a))
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO) via StateC Int (LiftC (InputT m))

instance MonadTrans ReadlineC where
  lift m = ReadlineC $ \ l -> (,) l <$> lift m

#if MIN_VERSION_haskeline(0, 8, 0)
instance (MonadIO m, MonadMask m) => Algebra Readline (ReadlineC m) where
#else
instance MonadException m => Algebra Readline (ReadlineC m) where
#endif
  alg _ sig ctx = case sig of
    Prompt prompt -> ReadlineC $ \ line -> do
      str <- getInputLine @m (cyan <> prompt <> plain)
      pure (line + 1, ((line, str) <$ ctx))
      where cyan = "\ESC[1;36m\STX"
            plain = "\ESC[0m\STX"
    Print doc -> do
      s <- maybe 80 Size.width <$> liftIO size
      let docstream = layoutSmart (layoutOptions s) (doc <> line)
      (<$ ctx) <$> (liftIO . renderIO stdout $ docstream)
      where layoutOptions s = defaultLayoutOptions { layoutPageWidth = AvailablePerLine s 0.8 }
