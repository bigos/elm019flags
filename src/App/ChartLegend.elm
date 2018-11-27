module App.ChartLegend exposing (drawLegendChartReview, drawLegendDataPoint, drawLegendLimitOrange, drawLegendLimitRed, drawLegendLineSegment, drawLegendMaintenanceLog, drawLegendOutsideValid, drawLegendShape, drawLegendShapeGeneric, drawLegendTheoreticalLine, showLegend)

import App.Chart exposing (..)
import App.Model exposing (..)
import Geometry.Svg as Svg
import Html exposing (Html, a, br, button, div, h3, hr, i, span, text)
import Html.Attributes exposing (classList, href)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import String.Interpolate exposing (interpolate)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)


drawLegendLineSegment colour =
    let
        cy =
            87.5
    in
    Svg.scaleAbout (Point2d.fromCoordinates ( -75, cy ))
        2.0
        (Svg.lineSegment2d
            [ Attributes.stroke colour
            , Attributes.strokeWidth "1.2"
            ]
            (LineSegment2d.fromEndpoints
                ( Point2d.fromCoordinates
                    ( -90, cy )
                , Point2d.fromCoordinates
                    ( -75, cy )
                )
            )
        )


drawLegendLimitOrange =
    drawLegendLineSegment "orange"


drawLegendLimitRed =
    drawLegendLineSegment "red"


drawLegendTheoreticalLine =
    drawLegendLineSegment "grey"


drawLegendShapeGeneric fillColour strokeColour strokeWidth shapeFn =
    let
        referencePoint =
            ( -87, 87 )
    in
    Svg.scaleAbout (Point2d.fromCoordinates referencePoint)
        1.5
        (Svg.polygon2d
            [ Attributes.fill fillColour
            , Attributes.stroke strokeColour
            , Attributes.strokeWidth strokeWidth
            ]
            (Polygon2d.singleLoop (shapeFn (Point2d.fromCoordinates referencePoint)))
        )


drawLegendDataPoint colours =
    let
        fillColour =
            case List.head colours of
                Nothing ->
                    "pink"

                Just col ->
                    col
    in
    drawLegendShapeGeneric fillColour "black" "0.25" shape


drawLegendMaintenanceLog =
    drawLegendShapeGeneric "red" "black" "0.5" maintenanceShape


drawLegendChartReview =
    drawLegendShapeGeneric "blue" "black" "0.25" reviewShape


drawLegendOutsideValid =
    drawLegendShapeGeneric "yellow" "red" "0.75" shapeOutsideValid


drawLegendShape : Model -> LegendShape -> List (Svg Msg)
drawLegendShape model shape =
    case shape of
        LegendDataPoint colours analytes _ ->
            [ Svg.placeIn frameLegend (drawLegendDataPoint colours) ]

        LegendLimitOrange ->
            [ Svg.placeIn frameLegend drawLegendLimitOrange ]

        LegendLimitRed ->
            [ Svg.placeIn frameLegend drawLegendLimitRed ]

        LegendTheoreticalLine ->
            [ Svg.placeIn frameLegend drawLegendTheoreticalLine ]

        LegendMaintenanceLog ->
            [ Svg.placeIn frameLegend drawLegendMaintenanceLog ]

        LegendChartReview ->
            [ Svg.placeIn frameLegend drawLegendChartReview ]

        LegendOutsideValid _ ->
            [ Svg.placeIn frameLegend drawLegendOutsideValid ]


showLegend : Model -> Html Msg
showLegend model =
    div []
        [ div [ class "container" ]
            [ h3 [ class "center" ]
                [ text "Legend" ]
            ]
        , div [ class "" ]
            [ div
                [ class "container" ]
                [ div [ class "row" ]
                    (List.map
                        (\ld ->
                            div [ class "col-md-6" ]
                                [ div [ class "row", style "margin-bottom:1.5em" ]
                                    [ div [ class "col-md-1" ]
                                        [ Svg.svg
                                            [ height "25"
                                            , viewBox "0 0 25 25"
                                            ]
                                            [ Svg.g [] (drawLegendShape model ld.shape) ]
                                        ]
                                    , div
                                        [ class "col-md-5" ]
                                        [ text ld.description
                                        ]
                                    ]
                                ]
                        )
                        model.legend
                    )
                ]
            ]
        ]
