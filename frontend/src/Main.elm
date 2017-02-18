module Main exposing (..)

import App exposing (..)
import Html exposing (programWithFlags)


main : Program String Model Msg
main =
    programWithFlags
        { view = view
        , init = mockInit
        , update = update
        , subscriptions = subscriptions
        }
