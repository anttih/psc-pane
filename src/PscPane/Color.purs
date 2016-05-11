module PscPane.Color where

import Prelude
import Ansi.Codes ( EscapeCode(Graphics)
                  , GraphicsParam(PBackground, PForeground, Reset)
                  , Color(Black, Green, Yellow, Red, White)
                  , escapeCodeToString)

withColor :: Array GraphicsParam -> String -> String
withColor params s =
  escapeCodeToString (Graphics params) <> s <> escapeCodeToString (Graphics [Reset])

green :: String -> String
green = withColor [PBackground Green, PForeground White]

yellow :: String -> String
yellow = withColor [PBackground Yellow, PForeground Black]

red :: String -> String
red = withColor [PBackground Red, PForeground White]

