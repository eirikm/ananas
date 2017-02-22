module App exposing (..)

import Helpers exposing (focus, onEnter)
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (autofocus, id, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (at)
import Json.Encode as Encode
import RemoteData exposing (RemoteData)


type alias Model =
    { username : String
    , usernameSubmitted : Bool
    , forrigeRegnestykke : Maybe BesvartRegnestykke
    , regnestykke : RemoteData String Regnestykke
    , riktigePåRad : Int
    , highscore : Int
    , svar : String
    }


type alias BesvartRegnestykke =
    { regnestykke : Regnestykke
    , svarFraBruker : Int
    , korrekt : Bool
    }


init : String -> ( Model, Cmd Msg )
init path =
    { username = ""
    , usernameSubmitted = False
    , forrigeRegnestykke = Nothing
    , regnestykke = RemoteData.NotAsked
    , riktigePåRad = 0
    , highscore = 0
    , svar = ""
    }
        ! []


mockInit : String -> ( Model, Cmd Msg )
mockInit path =
    { username = "spiderboy"
    , usernameSubmitted = True
    , forrigeRegnestykke = Nothing
    , regnestykke = RemoteData.NotAsked
    , riktigePåRad = 0
    , highscore = 0
    , svar = ""
    }
        ! [ (hentRegnestykke "spiderboy") ]


baseServerUrl : String
baseServerUrl =
    ""
--    "http://localhost:1337"


type alias Regnestykke =
    { a : Int
    , op : String
    , b : Int
    }


type alias NyttRegnestykkeResponse =
    { forrigeRegnestykke : Maybe BesvartRegnestykke
    , nyttRegnestykke : Regnestykke
    }


type Msg
    = NoOp
    | UsernameChanged String
    | UsernameSubmitted
    | SvarChanged String
    | SvarSubmitted
    | HentRegnestykke
    | NyttRegnestykke NyttRegnestykkeResponse


handleMottattMattestykke : Result Http.Error NyttRegnestykkeResponse -> Msg
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


besvartRegnestykkeDecode : Decode.Decoder BesvartRegnestykke
besvartRegnestykkeDecode =
    Decode.map3 BesvartRegnestykke
        (at [ "regnestykke" ] regnestykkeDecoder)
        (at [ "svarFraBruker" ] Decode.int)
        (at [ "riktig" ] Decode.bool)


nyttRegnestykkeResponseDecoder : Decode.Decoder NyttRegnestykkeResponse
nyttRegnestykkeResponseDecoder =
    Decode.map2 NyttRegnestykkeResponse
        (at [ "forrigeRegnestykke" ] (Decode.maybe besvartRegnestykkeDecode))
        (at [ "nyttRegnestykke" ] regnestykkeDecoder)


hentRegnestykke : String -> Cmd Msg
hentRegnestykke username =
    let
        encoder =
            Encode.object [ ( "id", Encode.string username ) ]
    in
        HttpBuilder.post (baseServerUrl ++ "/pluss")
            |> HttpBuilder.withJsonBody encoder
            |> HttpBuilder.withExpect (Http.expectJson nyttRegnestykkeResponseDecoder)
            |> HttpBuilder.send handleMottattMattestykke


sendSvar : String -> String -> Cmd Msg
sendSvar username svar =
    let
        svarRequest =
            Encode.object
                [ ( "id", Encode.string username )
                , ( "svar", Encode.string svar )
                ]
    in
        HttpBuilder.post (baseServerUrl ++ "/pluss/svar")
            |> HttpBuilder.withJsonBody svarRequest
            |> HttpBuilder.withExpect (Http.expectJson nyttRegnestykkeResponseDecoder)
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
            { model | regnestykke = RemoteData.Loading } ! [ hentRegnestykke model.username ]

        NyttRegnestykke nyttRegnestykkeResponse ->
            let
                nyttRegnestykke =
                    nyttRegnestykkeResponse.nyttRegnestykke

                besvartRegnestykke =
                    nyttRegnestykkeResponse.forrigeRegnestykke
                    |> Maybe.map (\r ->
                        { regnestykke = r.regnestykke
                        , svarFraBruker = r.svarFraBruker
                        , korrekt = r.korrekt
                        }
                    )
            in
                { model |
                    forrigeRegnestykke = besvartRegnestykke
                    , regnestykke = RemoteData.Success nyttRegnestykke }
                    ! [ focus "svar" NoOp ]

        SvarChanged input ->
            { model | svar = input } ! []

        SvarSubmitted ->
            { model | svar = "" } ! [ sendSvar model.username model.svar ]


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
                [ text "..." ]

            RemoteData.Failure e ->
                [ text <| "Auda. Her skjedde det noe galt: " ++ e ]

            RemoteData.Success regnestykke ->
                [
--                viewHighscore model.highscore,
                viewRiktigePåRad model.riktigePåRad
                , viewForrigeRegnestykke model.forrigeRegnestykke
                , viewRegnestykke regnestykke model.svar
                ]
        )


viewHighscore : Int -> Html Msg
viewHighscore highscore =
    p []
        [ text ("Highscore: " ++ (toString highscore))
        ]


viewForrigeRegnestykke : Maybe BesvartRegnestykke -> Html Msg
viewForrigeRegnestykke forrigeRegnestykke =
    let
        viewBesvartRegnestykke : BesvartRegnestykke -> Html Msg
        viewBesvartRegnestykke besvartRegnestykke =
            text
                ((toString besvartRegnestykke.regnestykke.a)
                    ++ "+"
                    ++ (toString besvartRegnestykke.regnestykke.b)
                    ++ "="
                    ++ (toString besvartRegnestykke.svarFraBruker)
                    ++ " "
                    ++ if (besvartRegnestykke.korrekt) then
                        "Riktig"
                       else
                        "Feil"
                )
    in
        div []
            [ forrigeRegnestykke
                |> Maybe.map viewBesvartRegnestykke
                |> Maybe.withDefault (text "")
            ]


viewRiktigePåRad : Int -> Html Msg
viewRiktigePåRad påRad =
    div []
        [ text <|
            case påRad of
                0 ->
                    ""

                1 ->
                    ""

                _ ->
                    "Du har svart " ++ (toString påRad) ++ " riktige på rad."
        ]


viewRegnestykke : Regnestykke -> String -> Html Msg
viewRegnestykke regnestykke svar =
    div []
        [ p [] [ text <| regnestykkeToString regnestykke ]
        , input
            [ id "svar"
            , type_ "number"
            , onInput SvarChanged
            , onEnter SvarSubmitted
            , autofocus True
            , value svar
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
