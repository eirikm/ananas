module View exposing (..)

import Helpers exposing (onEnter)
import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (autofocus, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Models exposing (..)
import RemoteData

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
                [ viewHighscore model.score.highscore
                , viewRiktigePåRad model.score.current
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


