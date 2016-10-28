module PscPane.Color where

import Prelude
import Ansi.Codes ( EscapeCode(Graphics)
                  , GraphicsParam(PForeground, Reset)
                  , Color(Green, Yellow, Red)
                  , escapeCodeToString)

withColor ∷ Array GraphicsParam → String → String
withColor params s =
  escapeCodeToString (Graphics params) <> s <> escapeCodeToString (Graphics [Reset])

green ∷ String → String
green = withColor [PForeground Green]

yellow ∷ String → String
yellow = withColor [PForeground Yellow]

red ∷ String → String
red = withColor [PForeground Red]

