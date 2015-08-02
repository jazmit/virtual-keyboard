module VirtualKeyboard (Key, render, keyPresses) where

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


backspace : Key
backspace = {
    display = "⇦",
    width = 2,
    keycode = 8
    }


enter : Key
enter = {
    display = "↵",
    width = 3,
    keycode = 13
    }


-- ## The keyboard raw data as a literal
keys : List (List Key)
keys =
    let getKeyCode s = 
            case S.uncons s of
                Just (char, "") -> Char.toCode char
                _ -> nullKey.keycode
        key s = { display = s, width = 2, keycode = getKeyCode s }
        keycode k key = { key | keycode <- k }
        toKeys = S.split "" >> L.map key
        spacer = { display = "", width = 1, keycode = nullKey.keycode }
    in [           toKeys "qwertyuiop" ++ [backspace],
       [spacer] ++ toKeys "asdfghjkl"  ++ [enter],
       [shift]  ++ toKeys "zxcvbnm,."  ++ [shift],
       [{ display = "", width = 2, keycode = 32 }]
       ]


-- ## Click handling
taps : Mailbox Key
taps = mailbox nullKey


-- TODO: make one isShift function, and use a signal delay to generate wasShift
isShift : Signal Bool
isShift = (\key -> key.keycode == shift.keycode) <~ taps.signal


wasShift : Signal Bool
wasShift =
    let isShift wShift oldKey = oldKey == shift && not wShift
        step key (wShift, oldKey) = (isShift wShift oldKey, key)
        stateSig = foldp step (False, nullKey) taps.signal
    in  (\(wShift, _) -> wShift) <~ stateSig


keyPresses : Signal KeyCode
keyPresses =
    let keyPress isShift key = key.keycode - if isShift then 32 else 0
    in  keyPress <~ wasShift ~ taps.signal


onTouchStart : Signal.Address a -> a -> Attribute
onTouchStart addr msg =
  onWithOptions "touchstart" (Options True True) Json.Decode.value (\_ -> Signal.message addr msg)


-- ## Rendering functions
renderKey : Bool -> Key -> Html
renderKey isShift v = div
        [ class ("key grow-" ++ toString v.width),
          onTouchStart taps.address v]
        [ text <| if isShift then (S.toUpper v.display) else v.display ]


renderRow : Bool -> List Key -> Html
renderRow isShift row = div [class "row"] <| L.map (renderKey isShift) row


renderKeyboard : List Key -> Bool -> Html
renderKeyboard shortcuts isShift = div [class "keyboard"] <|
    L.map (renderRow isShift) (shortcuts :: keys)


-- Renders the keyboard, provided with a way to lookup shortcuts such
-- as diacritical marks
render : (KeyCode -> List Key) -> Signal Html
render lookup =
    let doRender keycode = lookup keycode |> renderKeyboard
    in  doRender <~ keyPresses ~ isShift


-- Simple main for running the keyboard without JS integration
main : Signal Html
main = render (\_ -> [])
