{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
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
import Control.Carrier.Reader
import Control.Effect.Readline
import Control.Monad.Catch (MonadMask(..))
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

runReadline :: (MonadIO m, MonadMask m) => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings (ReadlineC m) = runInputTWithPrefs prefs settings (runM (runReader (Line 0) m))

runReadlineWithHistory :: (MonadIO m, MonadMask m) => ReadlineC m a -> m a
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

instance (MonadIO m, MonadMask m) => Algebra Readline (ReadlineC m) where
  alg _ sig ctx = case sig of
    Prompt prompt -> ReadlineC $ do
      str <- sendM (getInputLine @m (cyan <> prompt <> plain))
      Line line <- ask
      local increment $ pure ((line, str) <$ ctx)
      where cyan = "\ESC[1;36m\STX"
            plain = "\ESC[0m\STX"
    Print doc -> do
      s <- maybe 80 Size.width <$> liftIO size
      let docstream = layoutSmart (layoutOptions s) (doc <> line)
      (<$ ctx) <$> (liftIO . renderIO stdout $ docstream)
      where layoutOptions s = defaultLayoutOptions { layoutPageWidth = AvailablePerLine s 0.8 }


newtype Line = Line Int

increment :: Line -> Line
increment (Line n) = Line (n + 1)
