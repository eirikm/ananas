module Main exposing (..)

import App exposing (..)
import Html exposing (programWithFlags)
import Models exposing (Model, Msg)
import View exposing (view)


main : Program String Model Msg
main =
    programWithFlags
        { view = view
        , init = mockInit
        , update = update
        , subscriptions = subscriptions
        }
