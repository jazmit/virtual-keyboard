module VirtualKeyboardJSIntegration where

import VirtualKeyboard exposing(..)


port keyPressesOut : Signal Char
port keyPressesOut = keyPresses
