module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autofocus, style, value)
import Html.Events exposing (onInput, onBlur)
import Field exposing (..)
import FormFields exposing (..)
import Material
import Material.Scheme
import Material.Textfield as Textfield
import Material.Options as Options
import Material.List as Lists


type alias Model =
    { vesselName : Field String
    , yearBuilt : Field (Maybe Year)
    , yard : Field String
    , loa : Field Meters
    , beam : Field Meters
    , draftSummer : Field Meters
    , deadWeightSummer : Field Tons
    , tpi : Field TonsPerUnit
    , volume : Field FormFields.Volume
    , mdl : Material.Model
    }


type alias AltModel =
    { fields : List ( VepField, Field )
    }


type Msg
    = Update VepField String
    | Validate VepField
    | Mdl (Material.Msg Msg)


type VepField
    = VesselName
    | YearBuilt
    | Yard
    | LOA
    | Beam
    | DraftSummer
    | DeadWeightSummer
    | TonsPerInch
    | Volume


initialModel : Model
initialModel =
    { vesselName = vesselNameField
    , yearBuilt = yearBuiltField
    , yard = yardField
    , loa = loaField
    , beam = beamField
    , draftSummer = draftSummerField
    , deadWeightSummer = deadWeightSummerField
    , tpi = tpiField
    , volume = volumeField
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update VesselName input ->
            { model | vesselName = model.vesselName |> Field.updateRawValue input } ! []

        Update YearBuilt input ->
            { model | yearBuilt = model.yearBuilt |> Field.updateRawValue input } ! []

        Update Yard input ->
            { model | yard = model.yard |> Field.updateRawValue input } ! []

        Update LOA input ->
            { model | loa = model.loa |> Field.updateRawValue input } ! []

        Update Beam input ->
            { model | beam = model.beam |> Field.updateRawValue input } ! []

        Update DraftSummer input ->
            { model | draftSummer = model.draftSummer |> Field.updateRawValue input } ! []

        Update DeadWeightSummer input ->
            { model | deadWeightSummer = model.deadWeightSummer |> Field.updateRawValue input } ! []

        Update TonsPerInch input ->
            { model | tpi = model.tpi |> Field.updateRawValue input } ! []

        Update Volume input ->
            { model | volume = model.volume |> Field.updateRawValue input } ! []

        Validate VesselName ->
            { model | vesselName = model.vesselName |> Field.validate } ! []

        Validate Yard ->
            { model | yard = model.yard |> Field.validate } ! []

        Validate YearBuilt ->
            { model | yearBuilt = model.yearBuilt |> Field.validate } ! []

        Validate LOA ->
            { model | loa = model.loa |> Field.validate } ! []

        Validate Beam ->
            { model | beam = model.beam |> Field.validate } ! []

        Validate DraftSummer ->
            { model | draftSummer = model.draftSummer |> Field.validate } ! []

        Validate DeadWeightSummer ->
            { model | deadWeightSummer = model.deadWeightSummer |> Field.validate } ! []

        Validate TonsPerInch ->
            { model | tpi = model.tpi |> Field.validate } ! []

        Validate Volume ->
            { model | volume = model.volume |> Field.validate } ! []

        Mdl msg_ ->
            Material.update msg_ model


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }


additionalViewParams : { autofocus : Bool, suffix : String }
additionalViewParams =
    { autofocus = False
    , suffix = ""
    }


viewTextfield : Model -> Html Msg
viewTextfield model =
    Textfield.render Mdl
        [ 0 ]
        model.mdl
        [ Textfield.label "Vessel name"
        , Textfield.floatingLabel
        , Textfield.text_
        , Textfield.onInput (Update VesselName)
        , Textfield.onBlur (Validate VesselName)
        , Field.errorMsg model.vesselName
            |> Maybe.map Textfield.error
            |> Maybe.withDefault Options.nop
        ]


mdlTextfield :
    { modelNo : Int
    , mdl : Material.Model
    , field : Field a
    , vepField : VepField
    }
    -> Html Msg
mdlTextfield params =
    let
        textFieldHtml : Html Msg
        textFieldHtml =
            Textfield.render
                Mdl
                [ params.modelNo ]
                params.mdl
                [ Field.label params.field
                    |> Maybe.withDefault ""
                    |> Textfield.label
                , Textfield.floatingLabel
                , Textfield.text_
                , Textfield.onInput (Update params.vepField)
                , Textfield.onBlur (Validate params.vepField)
                , Field.errorMsg params.field
                    |> Maybe.map Textfield.error
                    |> Maybe.withDefault Options.nop
                ]

        suffix =
            Field.suffix params.field
                |> Maybe.withDefault ""

        around =
            Lists.li []
                [ Lists.content []
                    [ textFieldHtml
                    ]
                , Lists.content2 []
                    [ Lists.info2 [] [ text suffix ] ]
                ]
    in
        around

view : Model -> Html Msg
view model =
    div
        [ style [ ( "padding", "2rem" ) ] ]
        [ Lists.ul []
            [ mdlTextfield
                { modelNo = 0
                , mdl = model.mdl
                , field = model.vesselName
                , vepField = VesselName
                }
            , mdlTextfield
                { modelNo = 1
                , mdl = model.mdl
                , field = model.yearBuilt
                , vepField = YearBuilt
                }
            , mdlTextfield
                { modelNo = 2
                , mdl = model.mdl
                , field = model.yard
                , vepField = Yard
                }
            , mdlTextfield
                { modelNo = 3
                , mdl = model.mdl
                , field = model.loa
                , vepField = LOA
                }
            , mdlTextfield
                { modelNo = 4
                , mdl = model.mdl
                , field = model.beam
                , vepField = Beam
                }
            , mdlTextfield
                { modelNo = 5
                , mdl = model.mdl
                , field = model.draftSummer
                , vepField = DraftSummer
                }
            , mdlTextfield
                { modelNo = 6
                , mdl = model.mdl
                , field = model.deadWeightSummer
                , vepField = DeadWeightSummer
                }
            , mdlTextfield
                { modelNo = 7
                , mdl = model.mdl
                , field = model.tpi
                , vepField = DeadWeightSummer
                }
            ]
        ]
        |> Material.Scheme.top



--    table []
--        , case (Field.validatedValue model.tpi) of
--            Just (Result.Ok (TPC d)) ->
--                viewField
--                    { field = model.tpi
--                    , label = "Tons per cm"
--                    , updateMsg = Update TonsPerInch
--                    , validateMsg = Validate TonsPerInch
--                    } { additionalViewParams | suffix = "tpc" }
--            _ ->
--                viewField
--                    { field = model.tpi
--                    , label = "Tons per inch"
--                    , updateMsg = Update TonsPerInch
--                    , validateMsg = Validate TonsPerInch
--                    } { additionalViewParams | suffix = "tpi" }
--        , case (Field.validatedValue model.volume) of
--            Just (Result.Ok (CubicMeter d)) ->
--                viewField
--                    { field = model.volume
--                    , label = "Volume"
--                    , updateMsg = Update Volume
--                    , validateMsg = Validate Volume
--                    } { additionalViewParams | suffix = "m3" }
--            _ ->
--                viewField
--                    { field = model.volume
--                    , label = "Volume"
--                    , updateMsg = Update Volume
--                    , validateMsg = Validate Volume
--                    } { additionalViewParams | suffix = "cuft" }
--        ]


viewField :
    { field : Field a
    , label : String
    , updateMsg : String -> Msg
    , validateMsg : Msg
    }
    -> { autofocus : Bool
       , suffix : String
       }
    -> Html Msg
viewField params optionalParams =
    viewTableRow
        [ text params.label
        , input
            [ value (Field.rawValue params.field)
            , onInput params.updateMsg
            , onBlur params.validateMsg
            , autofocus optionalParams.autofocus
            ]
            []
        , text optionalParams.suffix
        , text
            (Field.errorMsg params.field
                |> Maybe.withDefault ""
            )
        ]


viewTableRow : List (Html Msg) -> Html Msg
viewTableRow list =
    tr [] <| List.map (\e -> td [] [ e ]) list
