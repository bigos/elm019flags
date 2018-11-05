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
            ( { model | textfieldMenuPlaceholder = "Select Machine" }, getMachines )

        RequestedMachines res ->
            Debug.log ("zzzz " ++ Debug.toString res)
                (let
                    opts1 =
                        case res of
                            Err _ ->
                                []

                            Ok d ->
                                d

                    opts =
                        List.map (\r -> Selectize.entry r.name) opts1
                 in
                 ( { model
                    | combinedAdditionStage = Just StageMachine
                    , textfieldMenu =
                        Selectize.closed "textfield-menu" identity opts
                   }
                 , Cmd.none
                 )
                )

        TextfieldMenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTextfieldOption
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
                    Selectize.update SelectButtonOption
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

        SelectTextfieldOption newSelection ->
            Debug.log ("menu option selection " ++ Debug.toString newSelection)
                (case model.combinedAdditionStage of
                    Nothing ->
                        ( { model | textfieldSelection = newSelection }, Cmd.none )

                    Just stage ->
                        case stage of
                            StageMachine ->
                                ( { model
                                    | textfieldSelection = newSelection

                                    -- we need different way of handline the machine data so we can use ieds here
                                    , combinedAdditionMachine = Just 1
                                    , combinedAdditionStage = Just StageSample
                                  }
                                , Cmd.none
                                )

                            _ ->
                                ( { model | textfieldSelection = newSelection }, Cmd.none )
                )

        SelectButtonOption newSelection ->
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
