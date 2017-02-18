module App exposing (..)

import Html exposing (Html, button, div, h1, img, input, text)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { username : String
    , usernameSubmitted : Bool
    }


init : String -> ( Model, Cmd Msg )
init path =
    { username = ""
    , usernameSubmitted = False
    }
        ! []


type Msg
    = NoOp
    | UsernameChanged String
    | UsernameSubmitted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UsernameChanged username ->
            { model | username = username } ! []

        UsernameSubmitted ->
            { model | usernameSubmitted = True } ! []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Ananas" ]
        , if model.usernameSubmitted then
            viewMattestykke model
          else
            viewInnlogging model
        ]


viewMattestykke : Model -> Html Msg
viewMattestykke model =
    div []
        [ text ("Velkommen, " ++ model.username ++ "!") ]


viewInnlogging : Model -> Html Msg
viewInnlogging model =
    div []
        [ text "Navn"
        , input [ onInput UsernameChanged ] []
        , button [ onClick UsernameSubmitted ] [ text "OK" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
