module VirtualKeyboardJSIntegration where

import VirtualKeyboard exposing(..)
import Html exposing (..)


port keyPressesOut : Signal Int
port keyPressesOut = keyPresses

main : Signal Html
main = VirtualKeyboard.main 
