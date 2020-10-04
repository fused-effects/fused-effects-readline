{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Readline
( -- * Readline effect
  Readline(..)
, getInputLine
, getInputLineWithInitial
, getInputChar
, getPassword
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

getInputChar :: Has Readline sig m => String -> m (Maybe Char)
getInputChar p = send (GetInputChar p)

getPassword :: Has Readline sig m => Maybe Char -> String -> m (Maybe String)
getPassword c s = send (GetPassword c s)

print :: Has Readline sig m => Doc AnsiStyle -> m ()
print s = send (Print s)


data Readline (m :: Type -> Type) (k :: Type) where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  GetInputChar :: String -> Readline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Readline m (Maybe String)
  Print :: Doc AnsiStyle -> Readline m ()
