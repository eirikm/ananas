module Helpers exposing (..)

import Dom
import Html
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode
import Task

onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Decode.andThen isEnter)


focus : String -> msg -> Cmd msg
focus id noopMsg =
    Dom.focus id
        |> Task.attempt (\_ -> noopMsg)


