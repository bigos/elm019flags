module App.Chart exposing (axisX, axisY, calcEt, chartElements, createMaintenanceLine, createMaintenanceShape, createMaxLine, createMeanLine, createMinLine, createNominalLine, createOrangeMeanLine, createOrangeXsdLine, createQcShape, createReviewLine, createReviewShape, createXsdLine, frameAxisX, frameAxisY, frameChart, frameLegend, genericShape, maintenanceShape, reviewShape, shape)

import App.ChartTicks exposing (..)
import App.Model exposing (..)
import App.Utilities exposing (..)
import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html.Events.Extra.Mouse as M exposing (..)
import ISO8601
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events as Events exposing (..)


axisX : Model -> Svg msg
axisX model =
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( 0.0
                , chartBottom model
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (chartEnd model.flags)
                , chartBottom model
                )
            )
        )


axisY : Model -> Svg msg
axisY model =
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( 0.0
                , chartBottom model
                )
            , Point2d.fromCoordinates
                ( 0.0
                , chartTop model
                )
            )
        )


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


createQcShape : Model -> ScaledPoint -> String -> Svg Msg
createQcShape model point fill =
    let
        myToolTip =
            if model.chartType == "default" then
                DataScaledPoint point

            else
                DataCombinedPoint point
    in
    Svg.polygon2d
        [ Attributes.fill fill
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter myToolTip event.pagePos Nothing)
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


createMaintenanceLine : Model -> ChartRecord -> Svg Msg
createMaintenanceLine model ml =
    let
        oni =
            toFloat (timify ml.on)

        ld =
            deviations model lodev List.minimum

        ud =
            deviations model 4.7 List.maximum
    in
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "1"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings ld )
            , Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings ud )
            )
        )


createReviewLine : Model -> ChartRecord -> Svg Msg
createReviewLine model ml =
    let
        oni =
            toFloat (timify ml.on)

        ld =
            deviations model lodev List.minimum

        ud =
            deviations model 4.5 List.maximum
    in
    Svg.lineSegment2d
        [ Attributes.stroke "green"
        , Attributes.strokeWidth "1"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings ld )
            , Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings ud )
            )
        )


createMaintenanceShape : Model -> ChartRecord -> Svg Msg
createMaintenanceShape model ml =
    let
        oni =
            toFloat (timify ml.on)

        ud =
            deviations model 4.7 List.maximum

        point =
            Point2d.fromCoordinates ( doX model.chartScalings oni, doY model.chartScalings ud )
    in
    Svg.polygon2d
        [ Attributes.fill "red"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter (DataChartRecord ml) event.pagePos (Just "Maintnance Log"))
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (maintenanceShape point))


createReviewShape : Model -> ChartRecord -> Svg Msg
createReviewShape model r =
    let
        oni =
            toFloat (timify r.on)

        ud =
            deviations model 4.5 List.maximum

        point =
            Point2d.fromCoordinates ( doX model.chartScalings oni, doY model.chartScalings ud )
    in
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter (DataChartRecord r) event.pagePos (Just "Review"))
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (reviewShape point))


createMeanLine : Model -> ( SectionData, Maybe SectionData ) -> Svg msg
createMeanLine model timeSection =
    let
        fixed =
            (Tuple.first timeSection).fixed

        st =
            toFloat
                (Basics.max (Tuple.first timeSection).start (timify model.flags.date_from))

        ted =
            timify model.flags.date_to + oneDay

        et =
            calcEt timeSection ted

        sd =
            findStatForTime model.flags.stats (round st)

        dmean =
            if fixed then
                -- TODO make it guarantee not to be shown at all
                0.0

            else
                case sd of
                    Nothing ->
                        (dataStats model).mean

                    Just v ->
                        v.mean
    in
    Svg.lineSegment2d
        [ Attributes.stroke "red"
        , Attributes.strokeWidth "1"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dmean )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dmean )
            )
        )


createOrangeMeanLine : Model -> DataStats -> Svg msg
createOrangeMeanLine model stats =
    let
        st =
            toFloat
                (timify model.flags.date_from)

        ted =
            timify model.flags.date_to + oneDay

        et =
            toFloat
                ted
    in
    Svg.lineSegment2d
        [ Attributes.stroke "orange"
        , Attributes.strokeWidth "1.2"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings stats.mean )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings stats.mean )
            )
        )


calcEt timeSection ted =
    toFloat
        (Basics.min ted
            (Maybe.withDefault { start = ted, fixed = False }
                (Tuple.second timeSection)
            ).start
        )


createNominalLine : Model -> ( SectionData, Maybe SectionData ) -> Svg msg
createNominalLine model timeSection =
    let
        st =
            toFloat
                (Basics.max (Tuple.first timeSection).start (timify model.flags.date_from))

        ted =
            timify model.flags.date_to + oneDay

        et =
            calcEt timeSection ted

        sd =
            findStatForTime model.flags.stats (round st)

        dmean =
            case sd of
                Nothing ->
                    (dataStats model).mean

                Just v ->
                    v.nominal
    in
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "2"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dmean )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dmean )
            )
        )


createMinLine : Model -> ( SectionData, Maybe SectionData ) -> Svg msg
createMinLine model timeSection =
    let
        st =
            toFloat
                (Basics.max (Tuple.first timeSection).start (timify model.flags.date_from))

        ted =
            timify model.flags.date_to + oneDay

        et =
            calcEt timeSection ted

        sd =
            findStatForTime model.flags.stats (round st)

        dmean =
            case sd of
                Nothing ->
                    0.0

                Just v ->
                    Maybe.withDefault 0.0
                        v.min
    in
    Svg.lineSegment2d
        [ Attributes.stroke "yellow"
        , Attributes.strokeWidth "3"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dmean )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dmean )
            )
        )


createMaxLine : Model -> ( SectionData, Maybe SectionData ) -> Svg msg
createMaxLine model timeSection =
    let
        st =
            toFloat
                (Basics.max (Tuple.first timeSection).start (timify model.flags.date_from))

        ted =
            timify model.flags.date_to + oneDay

        et =
            calcEt timeSection ted

        sd =
            findStatForTime model.flags.stats (round st)

        dmean =
            case sd of
                Nothing ->
                    0.0

                Just v ->
                    Maybe.withDefault 0.0 v.max
    in
    Svg.lineSegment2d
        [ Attributes.stroke "yellow"
        , Attributes.strokeWidth "3"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dmean )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dmean )
            )
        )


createXsdLine : Float -> Model -> ( SectionData, Maybe SectionData ) -> Svg msg
createXsdLine xsd model timeSection =
    let
        fixed =
            (Tuple.first timeSection).fixed

        st =
            toFloat
                (Basics.max (Tuple.first timeSection).start (timify model.flags.date_from))

        ted =
            timify model.flags.date_to + oneDay

        et =
            calcEt timeSection ted

        sd =
            findStatForTime model.flags.stats (round st)

        dmean =
            case sd of
                Nothing ->
                    (dataStats model).mean

                Just v ->
                    v.mean

        ddev =
            case sd of
                Nothing ->
                    (dataStats model).sd

                Just v ->
                    v.deviation

        dx =
            if fixed then
                -- TODO make it better, gaurantee it does not show on the chart
                0.0

            else
                dmean + xsd * ddev
    in
    Svg.lineSegment2d
        [ Attributes.stroke "red"
        , Attributes.strokeWidth "1"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dx )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dx )
            )
        )


createOrangeXsdLine : Float -> Model -> DataStats -> Svg msg
createOrangeXsdLine x model stats =
    let
        st =
            toFloat
                (timify model.flags.date_from)

        ted =
            timify model.flags.date_to + oneDay

        et =
            toFloat
                ted

        dx =
            stats.mean + (stats.sd * x)
    in
    Svg.lineSegment2d
        [ Attributes.stroke "orange"
        , Attributes.strokeWidth "1.2"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings st, doY model.chartScalings dx )
            , Point2d.fromCoordinates
                ( doX model.chartScalings et, doY model.chartScalings dx )
            )
        )


chartElements : Model -> List (Svg Msg)
chartElements model =
    let
        statsDateRanges =
            statStartTuples model

        orangeStats =
            if List.length model.flags.stats == 0 then
                [ dataStats model ]

            else
                []
    in
    [ Svg.placeIn frameChart (axisX model)
    , Svg.placeIn frameChart (axisY model)
    ]
        ++ List.map (\ml -> Svg.placeIn frameChart (createMaintenanceLine model ml)) model.flags.maintenance_logs
        ++ List.map (\ml -> Svg.placeIn frameChart (createMaintenanceShape model ml)) model.flags.maintenance_logs
        ++ List.map (\r -> Svg.placeIn frameChart (createReviewLine model r)) model.flags.reviews
        ++ List.map (\r -> Svg.placeIn frameChart (createReviewShape model r)) model.flags.reviews
        -- functions using statStartTuples work with single analyte charts
        ++ List.map (\s -> Svg.placeIn frameChart (createNominalLine model s)) statsDateRanges
        -- red stats lines
        ++ List.map (\s -> Svg.placeIn frameChart (createMeanLine model s)) statsDateRanges
        ++ List.map (\s -> Svg.placeIn frameChart (createXsdLine 3.0 model s)) statsDateRanges
        ++ List.map (\s -> Svg.placeIn frameChart (createXsdLine 2.0 model s)) statsDateRanges
        ++ List.map (\s -> Svg.placeIn frameChart (createXsdLine -2.0 model s)) statsDateRanges
        ++ List.map (\s -> Svg.placeIn frameChart (createXsdLine -3.0 model s)) statsDateRanges
        -- min max lines
        ++ List.map (\s -> Svg.placeIn frameChart (createMinLine model s)) statsDateRanges
        ++ List.map (\s -> Svg.placeIn frameChart (createMaxLine model s)) statsDateRanges
        -- orange stats lines will go here
        ++ List.map (\s -> Svg.placeIn frameChart (createOrangeMeanLine model s)) orangeStats
        ++ List.map (\s -> Svg.placeIn frameChart (createOrangeXsdLine 3.0 model s)) orangeStats
        ++ List.map (\s -> Svg.placeIn frameChart (createOrangeXsdLine 2.0 model s)) orangeStats
        ++ List.map (\s -> Svg.placeIn frameChart (createOrangeXsdLine -2.0 model s)) orangeStats
        ++ List.map (\s -> Svg.placeIn frameChart (createOrangeXsdLine -3.0 model s)) orangeStats
        -- data points
        ++ flatten
            (List.map2
                (\pl c ->
                    List.map
                        (\p -> Svg.placeIn frameChart (createQcShape model p c))
                        pl
                )
                model.scaledPoints
                [ "blue", "red", "black", "green", "yellow", "pink" ]
            )
        ++ List.map (\ys -> Svg.placeIn frameChart (createYearTicks model ys)) model.flags.axes.axis_x.year_starts
        ++ List.map (\ms -> Svg.placeIn frameChart (createMonthTicks model ms)) model.flags.axes.axis_x.month_starts
        ++ List.map (\ms -> Svg.placeIn frameChart (createWeekTicks model ms)) (weekTickVals model)
        ++ List.map (\ms -> Svg.placeIn frameChart (createDayTicks model ms)) (dayTickVals model)
        ++ List.map (\mt -> Svg.placeIn frameChart (createMajorTick model mt)) (majorYticks model)
        ++ List.map (\mt -> Svg.placeIn frameChart (createMinorTick model mt)) (minorYticks model)
