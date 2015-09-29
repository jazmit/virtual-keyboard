-- Javascript integration for the virtual keyboard
module VirtualKeyboardJSIntegration where

import VirtualKeyboard exposing (Key)
import Html exposing (..)
import Char exposing (KeyCode)
import String as S
import List as L
import Json.Decode exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Signal


-- Inputs 
port keyPressesIn : Signal Int
port shortcutsRaw : Value
port isTouch : Bool


-- Outputs
port keyPressesOut : Signal Int
port keyPressesOut = VirtualKeyboard.keyPresses


shortcutDecoder : Decoder (Dict String String)
shortcutDecoder = dict string


shortcuts : List (String, String)
shortcuts = case decodeValue shortcutDecoder shortcutsRaw of
                Ok val -> Dict.toList val
                Err msg -> []


-- Lookup a diacritical mark accent shortcut by key code
lookup : KeyCode -> List Key 
lookup keycode =
    let relevant (k, _) = L.head (S.toList k) == Just (Char.fromCode keycode)
        makeKey (k, v) = {width = 1, keycode = Char.toCode (withDefault 'X' <| L.head <| L.drop 1 <| S.toList k), display = v}
    in  L.map makeKey (L.filter relevant shortcuts)


main : Signal Html
main = VirtualKeyboard.render isTouch keyPressesIn lookup 
