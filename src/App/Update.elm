module App.Update exposing (getMachines, machineDecoder, update)

import App.AnalyteSelector exposing (..)
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
            let
                opts =
                    case res of
                        Err _ ->
                            []

                        Ok d ->
                            List.map Debug.toString d
            in
            Debug.log ("zzzz " ++ Debug.toString res)
                ( model
                , Cmd.none
                )

        Keypress str ->
            Debug.log ("pressed key " ++ Debug.toString str)
                ( model, Cmd.none )

        TextfieldMenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTextfieldMachine
                        model.analyteSelector.machineSelection
                        model.analyteSelector.machineMenu
                        selectizeMsg

                newModel =
                    let
                        mas =
                            model.analyteSelector

                        mas2 =
                            { mas | machineMenu = newMenu }
                    in
                    { model | analyteSelector = mas2 }

                cmd =
                    menuCmd |> Cmd.map TextfieldMenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                        |> andDo cmd

                Nothing ->
                    ( newModel, cmd )


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
