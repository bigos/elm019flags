-- code refactoring inspired by
-- https://github.com/halfzebra/elm-examples/blob/master/examples/fractal-architecture/src/Main.elm


module Main exposing (createMinorTick, createQcShape, dayTickVals, findTicks, findTicks1, frameAxisX, frameAxisY, frameChart, frameLegend, genericShape, hidev, justTimeString, justValFn, lodev, main, maintenanceShape, monthNumName, pdfLink, reviewShape, shape, showTheTooltip, spacedRange, subscriptions, update, view, weekTickVals)

import App.Model exposing (..)
import App.Utilities exposing (..)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, a, br, button, div, span, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as M exposing (..)
import ISO8601
import Json.Encode as E
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events as Events exposing (..)
import Triangle2d exposing (Triangle2d)
import Tuple



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MAJORITY OF TYPE DECLARATIONS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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



-- JUST FUNCTIONS


justTimeString : Maybe ISO8601.Time -> String
justTimeString tv =
    case tv of
        Nothing ->
            ""

        Just tm ->
            ISO8601.toString tm


justValFn v fn =
    case v of
        Nothing ->
            0

        Just n ->
            fn n



-- OTHER FUNCTIONS


hidev =
    4


lodev =
    -4.5



-- convert prescaled value to scaled one


frameChart : Frame2d
frameChart =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 75, 300 ))
        |> Frame2d.reverseY


frameAxisX : Frame2d
frameAxisX =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 75, 350 ))
        |> Frame2d.reverseY


frameAxisY : Frame2d
frameAxisY =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 25, 300 ))
        |> Frame2d.reverseY


frameLegend : Frame2d
frameLegend =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 500, 100 ))
        |> Frame2d.reverseY


createQcShape : ScaledPoint -> Svg Msg
createQcShape point =
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter (DataScaledPoint point) event.pagePos Nothing)
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (shape point.point2d))


genericShape : Point2d -> Float -> List ( Float, Float ) -> List Point2d
genericShape point scale shapeCoordinates =
    let
        pointPair =
            ( Point2d.xCoordinate point, Point2d.yCoordinate point )

        scaledShapeCoordinates =
            List.map
                (\p -> ( Tuple.first p * scale, Tuple.second p * scale ))
                shapeCoordinates

        pc =
            List.map
                (\sc ->
                    ( Tuple.first pointPair + Tuple.first sc
                    , Tuple.second pointPair + Tuple.second sc
                    )
                )
                scaledShapeCoordinates
    in
    List.map
        (\c ->
            Point2d.fromCoordinates
                ( (Tuple.first pointPair + Tuple.first c) / 2.0
                , (Tuple.second pointPair + Tuple.second c) / 2.0
                )
        )
        pc


shape : Point2d -> List Point2d
shape point =
    -- draw tilted square around point coordinates
    genericShape point 4.5 [ ( 0.0, 1.0 ), ( 1.0, 0.0 ), ( 0.0, -1.0 ), ( -1.0, 0.0 ) ]


maintenanceShape : Point2d -> List Point2d
maintenanceShape point =
    genericShape point
        10.5
        [ ( 0.0, 1.0 ), ( 1.0, 0.0 ), ( 0.0, -1.0 ), ( -1.0, 0.0 ) ]


reviewShape : Point2d -> List Point2d
reviewShape point =
    genericShape point
        10.0
        [ ( 0.0, 0.0 )
        , ( 1.0, 1.0 )
        , ( 1.0, 0.0 )
        , ( 0.0, -1.0 )
        , ( -1.0, 0.0 )
        , ( -1.0, 1.0 )
        ]


monthNumName : Int -> Maybe String
monthNumName monthPartNumber =
    List.Extra.getAt
        (monthPartNumber - 1)
        [ "Jan" -- year is in place of Jan
        , "Feb"
        , "Mar"
        , "Apr"
        , "May"
        , "Jun"
        , "Jul"
        , "Aug"
        , "Sep"
        , "Oct"
        , "Nov"
        , "Dec"
        ]


createMinorTick model mt =
    let
        ox =
            chartStart model.flags
    in
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "0.75"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings ox, doY model.chartScalings mt )
            , Point2d.fromCoordinates
                ( doX model.chartScalings ox - 7, doY model.chartScalings mt )
            )
        )


weekTickVals model =
    let
        axis_x =
            model.flags.axes.axis_x

        first =
            model.flags.date_from

        last =
            model.flags.date_to

        weekMiliseconds =
            1000 * 3600 * 24 * 7

        afterMonday =
            spacedRange weekMiliseconds (timify axis_x.monday) (timify last)

        firstDay =
            timify first
    in
    if model.flags.axes.axis_x.weeks < 100 then
        List.filter (\d -> d >= firstDay) afterMonday

    else
        []


dayTickVals model =
    let
        first =
            model.flags.date_from

        last =
            model.flags.date_to

        dayMiliseconds =
            1000 * 3600 * 24

        diff =
            ISO8601.diff (ISO8601.fromTime (timify last)) (ISO8601.fromTime (timify first))

        days =
            diff // dayMiliseconds
    in
    if model.flags.axes.axis_x.days < 100 then
        spacedRange dayMiliseconds (timify first) (timify last)

    else
        []



-- ticks below the max value


findTicks1 : Float -> Float -> Float -> List Float -> List Float
findTicks1 val bound step acc =
    if (val < bound) || (List.length acc > 250) then
        acc

    else
        findTicks1 (val - step) bound step (val :: acc)


findTicks : Float -> Float -> Float -> List Float
findTicks val lbound step =
    findTicks1 val lbound step []


spacedRange : Int -> Int -> Int -> List Int
spacedRange spacing first last =
    List.range 0 ((last - first) // spacing)
        |> List.map (\n -> first + n * spacing)


pdfLink : Model -> Html Msg
pdfLink model =
    if model.flags.pdf then
        div [] []

    else
        a
            [ href
                ("/analytes/"
                    ++ Debug.toString model.flags.analyteid
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
                        ++ "px;"
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
                            tm =
                                ISO8601.fromTime (floor d.datum.time)

                            t2 =
                                ISO8601.toString tm

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
                )


view : Model -> Html Msg
view model =
    div [ style "border: solid yellow 1px;" ]
        [ div [ style "margin: auto ; width:700px" ]
            [ Svg.svg
                [ height "400"
                , viewBox "0 0 700 400"
                , style "border: solid #abc 1px;"
                ]
                []
            ]
        , showTheTooltip model
        , div [ style "margin-top: 2em; text-align: center;" ]
            [ pdfLink model
            ]
        , div [ style "height:1em;" ] []

        -- , div [] [ text (Debug.toString model.flags) ]
        ]
