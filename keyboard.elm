module VirtualKeyboard where

import Html exposing (..)
import Html.Attributes exposing (..)
import List as L exposing((::))
import String as S


type alias Key = {
    text : String,
    width : Int
}

-- ## Key construction functions
key : String -> Key
key s = { text = s, width = 2 }


width : Int -> Key -> Key
width n key = {key | width <- n}


toKeys : String -> List Key
toKeys = S.split "" >> L.map (\s -> { text = s, width = 2})


-- ## The keyboard raw data as a literal
keys : List (List Key)
keys = [                   toKeys "qwertyuiop" ++ [key "del"],
    [key "" |> width 1] ++ toKeys "asdfghjkl"  ++ [key "enter" |> width 3],
    [key "shift"]       ++ toKeys "zxcvbnm,."  ++ [key "shift" |> width 2],
    [key "space" |> width 10]
    ]


-- # Rendering functions
renderKey : Key -> Html
renderKey v = div
        [ class "key",
          style [("flex-grow", toString v.width)]]
        [text v.text]


renderRow : List Key -> Html
renderRow row = div [class "row"] <| List.map renderKey row


main : Html
main = div [class "keyboard"] <|
    L.map renderRow keys


