module App exposing (..)

import Helpers exposing (focus, onEnter)
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (autofocus, id)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (at)
import RemoteData exposing (RemoteData)


type alias Model =
    { username : String
    , usernameSubmitted : Bool
    , regnestykke : RemoteData String Regnestykke
    , svar : String
    , melding : Maybe String
    }


init : String -> ( Model, Cmd Msg )
init path =
    { username = ""
    , usernameSubmitted = False
    , regnestykke = RemoteData.NotAsked
    , svar = ""
    , melding = Nothing
    }
        ! []


mockInit : String -> ( Model, Cmd Msg )
mockInit path =
    { username = "spiderboy"
    , usernameSubmitted = True
    , regnestykke = RemoteData.NotAsked
    , svar = ""
    , melding = Nothing
    }
        ! []


type alias Regnestykke =
    { a : Int
    , op : String
    , b : Int
    }


type Msg
    = NoOp
    | UsernameChanged String
    | UsernameSubmitted
    | SvarChanged String
    | SvarSubmitted
    | HentRegnestykke
    | NyttRegnestykke Regnestykke


handleMottattMattestykke : Result Http.Error Regnestykke -> Msg
handleMottattMattestykke result =
    case result of
        Err error ->
            Debug.log (toString error) NoOp

        Ok regnestykke ->
            NyttRegnestykke regnestykke


regnestykkeDecoder : Decode.Decoder Regnestykke
regnestykkeDecoder =
    Decode.map3 Regnestykke
        (at [ "a" ] Decode.int)
        (at [ "op" ] Decode.string)
        (at [ "b" ] Decode.int)


hentRegnestykke : Cmd Msg
hentRegnestykke =
    HttpBuilder.get "http://localhost:1337/pluss"
        |> HttpBuilder.withExpect (Http.expectJson regnestykkeDecoder)
        |> HttpBuilder.send handleMottattMattestykke


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UsernameChanged username ->
            { model | username = username } ! []

        UsernameSubmitted ->
            { model | usernameSubmitted = True } ! []

        HentRegnestykke ->
            { model | regnestykke = RemoteData.Loading } ! [ hentRegnestykke ]

        NyttRegnestykke regnestykke ->
            { model | regnestykke = RemoteData.Success regnestykke }
                ! [ focus "svar" NoOp ]

        SvarChanged input ->
            { model | svar = input } ! []

        SvarSubmitted ->
            let
                regnestykke : Maybe Regnestykke
                regnestykke =
                    model.regnestykke
                        |> RemoteData.toMaybe
            in
                { model
                    | regnestykke = RemoteData.Loading
                    , melding =
                        regnestykke
                            |> Maybe.andThen (rettSvar model.svar)
                }
                    ! [ hentRegnestykke ]


rettSvar : String -> Regnestykke -> Maybe String
rettSvar svar regnestykke =
    let
        konvertertSvar : Maybe Int
        konvertertSvar =
            String.toInt svar
                |> Result.toMaybe
    in
        case konvertertSvar of
            Just i ->
                let
                    riktigSvar =
                        regnestykke.a + regnestykke.b
                        |> Debug.log "riktigSvar"

                    fjon = Debug.log "svar fra bruker: " i
                    riktig = i == riktigSvar
                in
                    Just
                        (if (i == riktigSvar) then
                            "Riktig!"
                         else
                            "Det var nok feil, men du skal få et nytt regnestykke allikevel"
                        )

            Nothing ->
                Just "Det du skrev er ikke et tall engang!"


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Ananas" ]
        , if model.usernameSubmitted then
            viewInnlogget model
          else
            viewInnlogging model
        ]


viewInnlogget : Model -> Html Msg
viewInnlogget model =
    div []
        (case model.regnestykke of
            RemoteData.NotAsked ->
                [ p [] [ text ("Velkommen, " ++ model.username ++ "!") ]
                , button [ onClick HentRegnestykke ] [ text "Gi meg et regnestykke" ]
                ]

            RemoteData.Loading ->
                [ text "venter på regnestykke" ]

            RemoteData.Failure e ->
                [ text <| "Auda. Her skjedde det noe galt: " ++ e ]

            RemoteData.Success regnestykke ->
                [ viewRegnestykke model.melding regnestykke ]
        )


viewRegnestykke : Maybe String -> Regnestykke -> Html Msg
viewRegnestykke melding regnestykke =
    div []
        [ p []
            [ melding
                |> Maybe.withDefault ""
                |> text
            ]
        , p [] [ text <| regnestykkeToString regnestykke ]
        , input
            [ id "svar"
            , onInput SvarChanged
            , onEnter SvarSubmitted
            , autofocus True
            ]
            []
        ]


regnestykkeToString : Regnestykke -> String
regnestykkeToString regnestykke =
    toString regnestykke.a ++ regnestykke.op ++ toString regnestykke.b


viewInnlogging : Model -> Html Msg
viewInnlogging model =
    div []
        [ text "Navn"
        , input
            [ autofocus True
            , onInput UsernameChanged
            , onEnter UsernameSubmitted
            ]
            []
        , button [ onClick UsernameSubmitted ] [ text "OK" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
