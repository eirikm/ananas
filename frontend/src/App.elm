module App exposing (..)

import Helpers exposing (focus, onEnter)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (at)
import Json.Encode as Encode
import Models exposing (..)
import RemoteData exposing (RemoteData)


init : String -> ( Model, Cmd Msg )
init path =
    { username = ""
    , usernameSubmitted = False
    , forrigeRegnestykke = Nothing
    , regnestykke = RemoteData.NotAsked
    , score = Score 0 0
    , svar = ""
    }
        ! []


mockInit : String -> ( Model, Cmd Msg )
mockInit path =
    { username = "spiderboy"
    , usernameSubmitted = True
    , forrigeRegnestykke = Nothing
    , regnestykke = RemoteData.NotAsked
    , score = Score 0 0
    , svar = ""
    }
        ! [ (hentRegnestykke "spiderboy") ]


baseServerUrl : String
baseServerUrl =
--    "http://localhost:1337"
    ""

handleMottattMattestykke : Result Http.Error NyttRegnestykkeResponse -> Msg
handleMottattMattestykke result =
    case result of
        Err error ->
            Debug.log (toString error) NoOp

        Ok regnestykke ->
            NyttRegnestykke regnestykke


scoreDecoder : Decode.Decoder Score
scoreDecoder =
    Decode.map2 Score
        (at [ "current" ] Decode.int)
        (at [ "highscore" ] Decode.int)


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
    Decode.map3 NyttRegnestykkeResponse
        (at [ "forrigeRegnestykke" ] (Decode.maybe besvartRegnestykkeDecode))
        (at [ "nyttRegnestykke" ] regnestykkeDecoder)
        (at [ "score" ] scoreDecoder)


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
                        |> Maybe.map
                            (\r ->
                                { regnestykke = r.regnestykke
                                , svarFraBruker = r.svarFraBruker
                                , korrekt = r.korrekt
                                }
                            )
            in
                { model
                    | forrigeRegnestykke = besvartRegnestykke
                    , regnestykke = RemoteData.Success nyttRegnestykke
                    , score = nyttRegnestykkeResponse.score
                }
                    ! [ focus "svar" NoOp ]

        SvarChanged input ->
            { model | svar = input } ! []

        SvarSubmitted ->
            { model | svar = "" } ! [ sendSvar model.username model.svar ]



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
