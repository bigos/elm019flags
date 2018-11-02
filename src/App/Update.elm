module App.Update exposing (getMachines, machineDecoder, update)

import App.Model exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Selectize



-- https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/Json-Decode-Pipeline


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TooltipMouseEnter tooltipData coordinates title ->
            ( { model | tooltip = Just (Tooltip tooltipData coordinates title) }
            , Cmd.none
            )

        TooltipMouseLeave ->
            ( { model | tooltip = Nothing }
            , Cmd.none
            )

        GetMachines ->
            ( model, getMachines )

        RequestedMachines res ->
            Debug.log ("zzzz " ++ Debug.toString res)
                ( model, Cmd.none )

        TextfieldMenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTextfieldLicense
                        model.textfieldSelection
                        model.textfieldMenu
                        selectizeMsg

                newModel =
                    { model | textfieldMenu = newMenu }

                cmd =
                    menuCmd |> Cmd.map TextfieldMenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                        |> andDo cmd

                Nothing ->
                    ( newModel, cmd )

        ButtonMenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectButtonLicense
                        model.buttonSelection
                        model.buttonMenu
                        selectizeMsg

                newModel =
                    { model | buttonMenu = newMenu }

                cmd =
                    menuCmd |> Cmd.map ButtonMenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                        |> andDo cmd

                Nothing ->
                    ( newModel, cmd )

        SelectTextfieldLicense newSelection ->
            ( { model | textfieldSelection = newSelection }, Cmd.none )

        SelectButtonLicense newSelection ->
            ( { model | buttonSelection = newSelection }, Cmd.none )


andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )



-- HTTP


getMachines : Cmd Msg
getMachines =
    Http.send
        RequestedMachines
        (Http.get
            "http://localhost:3000/machines"
            (Decode.list machineDecoder)
        )


machineDecoder : Decoder Machine
machineDecoder =
    Decode.succeed Machine
        |> required "eid" Decode.int
        |> required "name" Decode.string



-- |> required "exclude" Decode.int
-- |> required "manual" Decode.int
-- |> required "branchid" Decode.int
-- |> required "food" Decode.int
-- |> required "id" Decode.int
-- |> required "instrument_lod" Decode.int
