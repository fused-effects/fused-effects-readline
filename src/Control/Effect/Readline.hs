{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Readline
( Readline(..)
) where

import Control.Algebra
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import GHC.Generics (Generic1)

data Readline m k
  = Prompt String (Int -> Maybe String -> m k)
  | Print (Doc AnsiStyle) (m k)
  deriving (Functor, Generic1)

instance HFunctor Readline
instance Effect   Readline
