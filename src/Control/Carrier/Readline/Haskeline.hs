{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline carrier
  runReadline
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

runReadline :: MonadException m => Prefs -> Settings m -> ReadlineC m a -> m a
runReadline prefs settings (ReadlineC m) = runInputTWithPrefs prefs (coerce settings) (runM (runReader (Line 0) m))

newtype ReadlineC m a = ReadlineC (ReaderC Line (LiftC (InputT m)) a)
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadTrans ReadlineC where
  lift = ReadlineC . lift . lift . lift


newtype Line = Line Int
