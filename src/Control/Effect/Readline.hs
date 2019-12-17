module Control.Effect.Readline
( Readline(..)
) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

data Readline m k
  = Prompt String (Int -> Maybe String -> m k)
  | Print (Doc AnsiStyle) (m k)
