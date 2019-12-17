module Control.Carrier.Readline.Haskeline
( -- * Readline carrier
  ReadlineC(..)
  -- * Readline effect
, module Control.Effect.Readline
) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Readline
import System.Console.Haskeline

newtype ReadlineC m a = ReadlineC (ReaderC Int (LiftC (InputT m)) a)
