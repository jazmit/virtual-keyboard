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

-- ## The keyboard raw data as a literal
keys : List (List Key)
keys =
    let getKeyCode s = 
            case S.uncons s of
                Just (char, "") -> Char.toCode char
                _ -> nullKey.keycode
        key s = { display = s, width = 2, keycode = getKeyCode s }
        width w key   = { key | width <- w }
        keycode k key = { key | keycode <- k }
        toKeys = S.split "" >> L.map key
    in [                      toKeys "qwertyuiop" ++ [key "del"],
       [key "" |> width 1] ++ toKeys "asdfghjkl"  ++ [key "enter" |> width 3],
       [key "shift"]       ++ toKeys "zxcvbnm,."  ++ [key "shift"],
       [key "space" |> width 10]
       ]


-- ## Click handling
taps : Mailbox Key
taps = mailbox nullKey

-- # Send keycode out to JS
-- TODO: move to JS module
--port keypresses : Signal Int
--port keypresses = .keycode <~ taps.signal


-- ## Rendering functions
renderKey : Key -> Html
renderKey v = div
        [ class "key",
          style [("flex-grow", toString v.width)],
          onClick taps.address v]
        [text v.display]


renderRow : List Key -> Html
renderRow row = div [class "row"] <| L.map renderKey row


renderKeyboard : Html
renderKeyboard = div [class "keyboard"] <|
    L.map renderRow keys


main : Signal Html
main =
    let renderAll key =
        div [] [
            text (toString key),
            renderKeyboard
            ]
    in  renderAll <~ taps.signal


