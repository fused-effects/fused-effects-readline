{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Readline
( -- * Readline effect
  Readline(..)
, getInputLine
, getInputLineWithInitial
, print
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude hiding (print)

getInputLine :: Has Readline sig m => String -> m (Maybe String)
getInputLine p = send (GetInputLine p)

getInputLineWithInitial :: Has Readline sig m => String -> (String, String) -> m (Maybe String)
getInputLineWithInitial p lr = send (GetInputLineWithInitial p lr)

print :: Has Readline sig m => Doc AnsiStyle -> m ()
print s = send (Print s)


data Readline (m :: Type -> Type) (k :: Type) where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  Print :: Doc AnsiStyle -> Readline m ()
