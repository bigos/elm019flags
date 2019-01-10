module App.Update exposing (getMachines, machineDecoder, update)

import App.Model exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Selectize
import String.Interpolate exposing (interpolate)


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

        ResetCombinedAddition ->
            ( { model
                | textfieldMenuOptions = Nothing
                , textfieldMenuPlaceholder = "Waiting for Command"
                , combinedAdditionStage = Nothing
                , combinedAdditionMachine = Nothing
                , combinedAdditionSample = Nothing
                , combinedAdditionAnalyte = Nothing
              }
            , Cmd.none
            )

        GetMachines ->
            ( { model | textfieldMenuPlaceholder = "Select Machine" }, getMachines model )

        RequestedMachines res ->
            let
                opts1 =
                    case res of
                        Err _ ->
                            []

                        Ok d ->
                            d

                opts2 =
                    List.map (\r -> Tree (String.fromInt r.eid) r.name) opts1

                opts3 =
                    opts2
                        |> List.map Selectize.entry
                        |> Selectize.closed "textfield-menu" (\e -> e.name)
            in
            ( { model
                | combinedAdditionStage = Just StageMachine
                , textfieldMenu =
                    opts3
              }
            , Cmd.none
            )

        RequestedSamples res ->
            let
                opts1 =
                    case res of
                        Err _ ->
                            []

                        Ok d ->
                            d

                opts2 =
                    List.map (\r -> Tree (String.fromInt r.sampleid) r.name) opts1

                opts3 =
                    opts2
                        |> List.map Selectize.entry
                        |> Selectize.closed "textfield-menu" (\e -> e.name)
            in
            ( { model
                | combinedAdditionStage = Just StageSample
                , textfieldMenu =
                    opts3
              }
            , Cmd.none
            )

        RequestedAnalytes res ->
            let
                opts1 =
                    case res of
                        Err _ ->
                            []

                        Ok d ->
                            d

                opts2 =
                    List.map (\r -> Tree (String.fromInt r.analyteid) r.name) opts1

                opts3 =
                    opts2
                        |> List.map Selectize.entry
                        |> Selectize.closed "textfield-menu" (\e -> e.name)
            in
            ( { model
                | combinedAdditionStage = Just StageAnalyte
                , textfieldMenu =
                    opts3
              }
            , Cmd.none
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

        SelectTextfieldOption newSelection ->
            case model.combinedAdditionStage of
                Nothing ->
                    ( { model | textfieldSelection = newSelection }, Cmd.none )

                Just stage ->
                    case stage of
                        StageMachine ->
                            case newSelection of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just ns ->
                                    let
                                        mid =
                                            String.toInt ns.id
                                    in
                                    ( { model
                                        | combinedAdditionMachine = mid
                                        , combinedAdditionStage = Just StageSample
                                        , textfieldMenuPlaceholder = "Select Sample"
                                        , textfieldSelection = Nothing
                                        , textfieldMenuOptions = Nothing
                                      }
                                    , getSamples mid model
                                    )

                        StageSample ->
                            case newSelection of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just ns ->
                                    let
                                        mid =
                                            String.toInt ns.id
                                    in
                                    ( { model
                                        | combinedAdditionSample = mid
                                        , combinedAdditionStage = Just StageAnalyte
                                        , textfieldMenuPlaceholder = "Select Analyte"
                                        , textfieldSelection = Nothing
                                        , textfieldMenuOptions = Nothing
                                      }
                                    , getAnalytes mid model
                                    )

                        StageAnalyte ->
                            case newSelection of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just ns ->
                                    let
                                        boo =
                                            Debug.log ("zzzzzzzzzzzzzz " ++ Debug.toString ns) 1

                                        mid =
                                            String.toInt ns.id
                                    in
                                    ( { model
                                        | combinedAdditionAnalyte = mid
                                        , combinedAdditionAnalyteName = Just ns.name
                                        , combinedAdditionStage = Just StageAnalyteConfirmation
                                        , textfieldMenuPlaceholder = ""
                                        , textfieldSelection = Nothing
                                        , textfieldMenuOptions = Nothing
                                      }
                                    , Cmd.none
                                    )

                        StageAnalyteConfirmation ->
                            ( model, Cmd.none )


andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )



-- HTTP


getMachines : Model -> Cmd Msg
getMachines model =
    Http.send
        RequestedMachines
        (Http.get
            (interpolate "{0}/machines.json" [ model.flags.host ])
            (Decode.list machineDecoder)
        )


getSamples : Maybe Int -> Model -> Cmd Msg
getSamples mid model =
    let
        id =
            case mid of
                Nothing ->
                    -1

                Just maid ->
                    maid
    in
    Http.send
        RequestedSamples
        (Http.get
            (interpolate "{0}/machines/{1}/samples.json" [ model.flags.host, String.fromInt id ])
            (Decode.list sampleDecoder)
        )


getAnalytes : Maybe Int -> Model -> Cmd Msg
getAnalytes mid model =
    let
        id =
            case mid of
                Nothing ->
                    -1

                Just maid ->
                    maid
    in
    Http.send
        RequestedAnalytes
        (Http.get
            (interpolate "{0}/samples/{1}/analytes.json" [ model.flags.host, String.fromInt id ])
            (Decode.list analyteDecoder)
        )


machineDecoder : Decoder Machine
machineDecoder =
    Decode.succeed Machine
        |> required "eid" Decode.int
        |> required "name" Decode.string


sampleDecoder : Decoder Sample
sampleDecoder =
    Decode.succeed Sample
        |> required "sampleid" Decode.int
        |> required "name" Decode.string


analyteDecoder : Decoder Analyte
analyteDecoder =
    Decode.succeed Analyte
        |> required "analyteid" Decode.int
        |> required "name" Decode.string
