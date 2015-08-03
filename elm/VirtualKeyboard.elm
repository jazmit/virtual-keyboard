module VirtualKeyboard where -- (Key, render, keyPresses) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L exposing((::))
import String as S
import Maybe exposing (withDefault)
import Dict exposing (Dict)
import Signal exposing (..)
import Char exposing (KeyCode)
import Json.Decode


type alias Key = {
    display  : String,
    width    : Int,
    keycode  : KeyCode
}


nullKey : Key
nullKey = {
    display = "null",
    width   = 0,
    keycode = 0
    }


shift : Key
shift = {
    display = "⇑",
    width = 2,
    keycode = 16
    }

-- ## Generates the raw keyboard data
keyboard : Bool -> List (List Key)
keyboard isShift =
    let alphaKey s = key (Char.toLower s) (Char.toUpper s)
        alphaKeys  = S.toList >> L.map alphaKey
        key l u    =
            let c = if isShift then u else l
            in  { display = S.fromChar c, width = 2, keycode = Char.toCode c } 
        spacer    = { display = "",  width = 1, keycode = nullKey.keycode }
        enter     = { display = "↵", width = 3, keycode = 13 }
        backspace = { display = "⇦", width = 2, keycode = 8 }
    in [           alphaKeys "qwertyuiop" ++ [backspace],
       [spacer] ++ alphaKeys "asdfghjkl"  ++ [enter],
       [shift]  ++ alphaKeys "zxcvbnm" ++ [key ',' '!', key '.' '?', shift],
       [{ display = "Space", width = 2, keycode = 32 }]
       ]


-- ## Click handling
taps : Mailbox Key
taps = mailbox nullKey


isShift : Signal Bool
isShift = (\key -> key.keycode == shift.keycode) <~ taps.signal


keyPresses : Signal KeyCode
keyPresses = map .keycode taps.signal


onTouchStart : Signal.Address a -> a -> Attribute
onTouchStart addr msg =
  onWithOptions "touchstart" (Options True True) Json.Decode.value (\_ -> Signal.message addr msg)


-- ## Rendering functions
renderKey : Key -> Html
renderKey v = div
        [ class ("key grow-" ++ toString v.width),
          onClick taps.address v,
          onTouchStart taps.address v]
        [ text v.display ]


renderRow :  List Key -> Html
renderRow row = div [class "row"] <| L.map renderKey row


renderKeyboard : List Key -> Bool -> Html
renderKeyboard shortcuts isShift = div [class "keyboard"] <|
    L.map renderRow (shortcuts :: keyboard isShift)


-- Renders the keyboard, provided with a way to lookup shortcuts such
-- as diacritical marks
render : (KeyCode -> List Key) -> Signal Html
render lookup = (lookup >> renderKeyboard) <~ keyPresses ~ isShift


-- Simple main for running the keyboard without JS integration
main : Signal Html
main =
    let debugRender mainHtml keypress = div [] [
            text (toString keypress),
            mainHtml
        ]
    in  debugRender <~ render (\_ -> []) ~ map Char.fromCode keyPresses
