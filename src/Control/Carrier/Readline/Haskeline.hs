{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Readline.Haskeline
( -- * Readline carrier
  ReadlineC(..)
  -- * Readline effect
, module Control.Effect.Readline
) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Readline
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Console.Haskeline

newtype ReadlineC m a = ReadlineC (ReaderC Int (LiftC (InputT m)) a)
  deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadTrans ReadlineC where
  lift = ReadlineC . lift . lift . lift
