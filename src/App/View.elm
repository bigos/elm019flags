module App.View exposing (pdfLink, showTheTooltip, view)

import App.Chart exposing (..)
import App.ChartTicks exposing (..)
import App.Model exposing (..)
import App.Utilities exposing (..)
import Html exposing (Html, a, br, button, div, h3, hr, i, span, text)
import Html.Attributes exposing (classList, href)
import Html.Events exposing (onClick)
import ISO8601
import Selectize
import String.Interpolate exposing (interpolate)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)


showTheTooltip : Model -> Html Msg
showTheTooltip model =
    case model.tooltip of
        Nothing ->
            div [] []

        Just tt ->
            div
                [ class "log-record-tooltip"
                , style
                    ("left: "
                        ++ String.fromFloat (10 + Tuple.first tt.coordinates)
                        ++ "px; "
                        ++ "top: "
                        ++ String.fromFloat (10 + Tuple.second tt.coordinates)
                        ++ "px; max-width: 250px;"
                    )
                ]
                (case tt.data of
                    DataChartRecord d ->
                        [ span [ class "tool-tip-title" ] [ text (Maybe.withDefault "" tt.title) ]
                        , br [] []
                        , span [ class "tool-tip-title" ] [ text "On: " ]
                        , span [] [ text d.on ]
                        , br [] []
                        , span [ class "tool-tip-title" ] [ text "By: " ]
                        , span [] [ text d.by ]
                        , br [] []
                        , span [ class "tool-tip-title" ] [ text "Comment: " ]
                        , span [] [ text d.comment ]
                        ]

                    DataScaledPoint d ->
                        let
                            t2 =
                                ISO8601.toString <|
                                    ISO8601.fromTime (floor d.datum.time)

                            t3 =
                                List.head (String.split "Z" t2)

                            t4 =
                                String.split "T" (Maybe.withDefault "" t3)

                            dx =
                                List.head t4

                            tx =
                                case List.tail t4 of
                                    Nothing ->
                                        ""

                                    Just s ->
                                        Maybe.withDefault "" (List.head s)
                        in
                        [ span
                            [ class "tool-tip-title" ]
                            [ text "Date: " ]
                        , span [] [ text (Maybe.withDefault "" dx) ]
                        , br [] []
                        , span [ class "tool-tip-title" ] [ text "Time: " ]
                        , span [] [ text tx ]
                        , br [] []
                        , span [ class "tool-tip-title" ]
                            [ text "Concentration: " ]
                        , span
                            []
                            [ text (String.fromFloat d.datum.value) ]
                        ]

                    DataCombinedPoint d ->
                        let
                            t2 =
                                ISO8601.toString <|
                                    ISO8601.fromTime (floor d.datum.time)

                            t3 =
                                List.head (String.split "Z" t2)

                            t4 =
                                String.split "T" (Maybe.withDefault "" t3)

                            dx =
                                List.head t4

                            tx =
                                case List.tail t4 of
                                    Nothing ->
                                        ""

                                    Just s ->
                                        Maybe.withDefault "" (List.head s)

                            poisonId =
                                d.datum.aid

                            anarec =
                                Maybe.withDefault defaultAnalyteData
                                    (List.head
                                        (List.filter
                                            (\ax ->
                                                ax.id == poisonId
                                            )
                                            model.flags.analytes
                                        )
                                    )
                        in
                        [ span
                            [ class "tool-tip-title" ]
                            [ text "Analyte: " ]
                        , span
                            []
                            [ text anarec.analyte ]
                        , br [] []
                        , span
                            [ class "tool-tip-title" ]
                            [ text "EID: " ]
                        , span
                            []
                            [ text (String.fromInt anarec.eid) ]
                        , br [] []
                        , span
                            [ class "tool-tip-title" ]
                            [ text "Machine: " ]
                        , span
                            []
                            [ text anarec.machine ]
                        , br [] []
                        , span
                            [ class "tool-tip-title" ]
                            [ text "Sample: " ]
                        , span
                            []
                            [ text anarec.sample ]
                        , hr []
                            []
                        , span
                            [ class "tool-tip-title" ]
                            [ text "Date: " ]
                        , span [] [ text (Maybe.withDefault "" dx) ]
                        , br [] []
                        , span [ class "tool-tip-title" ] [ text "Time: " ]
                        , span [] [ text tx ]
                        , br [] []
                        , span [ class "tool-tip-title" ]
                            [ text "Concentration: " ]
                        , span
                            []
                            [ text (String.fromFloat d.datum.value) ]
                        ]
                )


pdfLink : Model -> Html Msg
pdfLink model =
    if model.flags.show_pdf_download then
        a
            [ href
                ("/analytes/"
                    ++ String.fromInt (singleAnalyteId model)
                    ++ "/pdf_report"
                    ++ "/chart_type/"
                    ++ model.chartType
                    ++ "/dating_from/"
                    ++ String.slice 0 10 (justTimeString model.dateFrom)
                    ++ "/dating_to/"
                    ++ String.slice 0 10 (justTimeString model.dateTo)
                )
            , target "_blank"
            ]
            [ text "Download the PDF" ]

    else
        div [] []


view : Model -> Html Msg
view model =
    let
        widthStr =
            "1200"

        heightStr =
            "800"
    in
    div [ style "border: solid yellow 1px;" ]
        [ div [ style ("margin: auto ; width:" ++ widthStr ++ "px") ]
            [ Svg.svg
                [ height heightStr
                , viewBox ("0 0 " ++ widthStr ++ " " ++ heightStr)
                , style "border: solid #abc 1px;"
                ]
                [ Svg.g [] (chartElements model) ]
            ]
        , showTheTooltip model
        , div [ style "margin-top: 2em; text-align: center;" ]
            [ pdfLink model
            ]
        , div [ style "height:1em;" ] []
        , hr [] []
        , if model.chartType == "default" then
            div [] []

          else
            combinedViewPart model
        , div [] [ text ("stage" ++ Debug.toString model.combinedAdditionStage) ]
        , div [] [ text ("machine" ++ Debug.toString model.combinedAdditionMachine) ]
        , div [] [ text ("sample" ++ Debug.toString model.combinedAdditionSample) ]
        , div [] [ text ("analyte" ++ Debug.toString model.combinedAdditionAnalyte) ]
        ]


newIdsLink : Model -> String
newIdsLink model =
    let
        aa =
            model.flags.analytes

        ids2 =
            List.map (\a -> a.id) aa

        ids1 =
            ids2
                ++ (case model.combinedAdditionAnalyte of
                        Nothing ->
                            []

                        Just a ->
                            [ a ]
                   )

        ids =
            String.join "," (List.map (\z -> String.fromInt z) ids1)
    in
    interpolate
        "http://{0}/analytes/combined/dating_from/{1}/dating_to/{2}/analyte_ids/{3}"
        [ "localhost:3000"
        , model.flags.date_from
        , model.flags.date_to
        , ids
        ]


combinedViewPart model =
    case model.combinedAdditionStage of
        Nothing ->
            div []
                [ div []
                    [ text "no particular addition at the moment"
                    ]
                , button [ onClick GetMachines ] [ text "Add Analyte - first step - machines" ]
                ]

        Just stage ->
            case stage of
                StageAnalyteConfirmation ->
                    div []
                        [ div [] [ text "analyte confirmation" ]
                        , a [ href (newIdsLink model) ] [ text ("Add analyute " ++ Debug.toString model.combinedAdditionAnalyte) ]
                        ]

                StageAnalyte ->
                    div []
                        [ div [] [ text "analyte stage" ]
                        , div
                            [ style "display: flex"
                            , style "flex-flow: column"
                            ]
                            [ div
                                [ class "container" ]
                                [ div []
                                    [ Html.map TextfieldMenuMsg <|
                                        Selectize.view
                                            (viewConfigTextfield
                                                model
                                            )
                                            model.textfieldSelection
                                            model.textfieldMenu
                                    ]
                                ]
                            ]
                        ]

                StageSample ->
                    div []
                        [ div [] [ text "sample stage" ]
                        , div
                            [ style "display: flex"
                            , style "flex-flow: column"
                            ]
                            [ div
                                [ class "container" ]
                                [ div []
                                    [ Html.map TextfieldMenuMsg <|
                                        Selectize.view
                                            (viewConfigTextfield
                                                model
                                            )
                                            model.textfieldSelection
                                            model.textfieldMenu
                                    ]
                                ]
                            ]
                        ]

                StageMachine ->
                    div []
                        [ div [] [ text "machine stage" ]
                        , div
                            [ style "display: flex"
                            , style "flex-flow: column"
                            ]
                            [ div
                                [ class "container" ]
                                [ div []
                                    [ Html.map TextfieldMenuMsg <|
                                        Selectize.view
                                            (viewConfigTextfield
                                                model
                                            )
                                            model.textfieldSelection
                                            model.textfieldMenu
                                    ]
                                ]
                            ]
                        ]



---- CONFIGURATION


viewConfigTextfield : Model -> Selectize.ViewConfig Tree
viewConfigTextfield model =
    viewConfig (textfieldSelector model)


viewConfig : Selectize.Input Tree -> Selectize.ViewConfig Tree
viewConfig selector =
    Selectize.viewConfig
        { container = []
        , menu =
            [ class "selectize__menu" ]
        , ul =
            [ class "selectize__list" ]
        , entry =
            \tree mouseFocused keyboardFocused ->
                { attributes =
                    [ class "selectize__item"
                    , classList
                        [ ( "selectize__item--mouse-selected"
                          , mouseFocused
                          )
                        , ( "selectize__item--key-selected"
                          , keyboardFocused
                          )
                        ]
                    ]
                , children =
                    [ text tree.name ]
                }
        , divider =
            \title ->
                { attributes =
                    [ class "selectize__divider" ]
                , children =
                    [ text title ]
                }
        , input = selector
        }


textfieldSelector : Model -> Selectize.Input Tree
textfieldSelector model =
    Selectize.autocomplete <|
        { attrs =
            \sthSelected open ->
                [ class "selectize__textfield"
                , classList
                    [ ( "selectize__textfield--selection", sthSelected )
                    , ( "selectize__textfield--no-selection", not sthSelected )
                    , ( "selectize__textfield--menu-open", open )
                    ]
                ]
        , toggleButton = toggleButton
        , clearButton = clearButton
        , placeholder = model.textfieldMenuPlaceholder
        }


toggleButton : Maybe (Bool -> Html Never)
toggleButton =
    Just <|
        \open ->
            div
                [ class "selectize__menu-toggle"
                , classList
                    [ ( "selectize__menu-toggle--menu-open", open ) ]
                ]
                [ i
                    [ class "material-icons"
                    , class "selectize__icon"
                    ]
                    [ if open then
                        text "arrow_drop_up"

                      else
                        text "arrow_drop_down"
                    ]
                ]


clearButton : Maybe (Html Never)
clearButton =
    Nothing
