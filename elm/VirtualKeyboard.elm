module VirtualKeyboard where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L exposing((::))
import String as S
import Signal exposing (..)
import Char


type alias Key = {
    display  : String,
    width    : Int,
    keycode  : Int
}

nullKey : Key
nullKey = {
    display = "null",
    width   = 0,
    keycode = 0
    }

shift : Key
shift = {
    display = "shift",
    width = 2,
    keycode = 16
    }

backspace : Key
backspace = {
    display = "del",
    width = 2,
    keycode = 8
    }

enter : Key
enter = {
    display = "Enter",
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
       [{ display = "Space", width = 2, keycode = 32 }]
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


keyPresses : Signal Int
keyPresses =
    let keyPress isShift key = key.keycode - if isShift then 32 else 0
    in  keyPress <~ wasShift ~ taps.signal


-- ## Rendering functions
renderKey : Bool -> Key -> Html
renderKey isShift v = div
        [ class "key",
          style [("flex-grow", toString v.width)],
          onClick taps.address v]
        [ text <| if isShift then (S.toUpper v.display) else v.display ]


renderRow : Bool -> List Key -> Html
renderRow isShift row = div [class "row"] <| L.map (renderKey isShift) row


renderKeyboard : Bool -> Html
renderKeyboard isShift = div [class "keyboard"] <|
    L.map (renderRow isShift) keys


main : Signal Html
main =
    let renderAll isShift key =
        div [] [
            text (toString key),
            renderKeyboard isShift
            ]
    in  renderAll <~ isShift ~ keyPresses


