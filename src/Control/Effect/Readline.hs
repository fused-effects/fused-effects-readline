{-# LANGUAGE DeriveFunctor, DeriveGeneric, GADTs, KindSignatures #-}
module Control.Effect.Readline
( -- * Readline effect
  Readline(..)
, prompt
, print
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Kind (Type)
import Prelude hiding (print)

prompt :: Has Readline sig m => String -> m (Int, Maybe String)
prompt p = send (Prompt p)

print :: Has Readline sig m => Doc AnsiStyle -> m ()
print s = send (Print s)


data Readline (m :: Type -> Type) (k :: Type) where
  Prompt :: String -> Readline m (Int, Maybe String)
  Print :: Doc AnsiStyle -> Readline m ()
