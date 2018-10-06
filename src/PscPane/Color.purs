module PscPane.Color where

import Prelude

import Ansi.Codes (EscapeCode(Graphics), GraphicsParam(PForeground, Reset), Color(Green, Yellow, Red), escapeCodeToString)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)

withColor ∷ NonEmptyList GraphicsParam → String → String
withColor params s =
  escapeCodeToString (Graphics params) <> s <> escapeCodeToString (Graphics (singleton Reset))

green ∷ String → String
green = withColor (singleton (PForeground Green))

yellow ∷ String → String
yellow = withColor (singleton (PForeground Yellow))

red ∷ String → String
red = withColor (singleton (PForeground Red))

