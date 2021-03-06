module App.ChartTicks exposing (createDayTicks, createMajorTick, createMinorTick, createMonthTicks, createWeekTicks, createYearTicks, dayTickVals, findTicks, findTicks1, majorYticks, minorYticks, spacedRange, weekTickVals)

import App.Model exposing (..)
import App.Utilities exposing (..)
import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (text)
import ISO8601
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)


createYearTicks : Model -> String -> Svg msg
createYearTicks model ys =
    let
        oni =
            toFloat (timify ys)

        yearPart =
            (ISO8601.fromTime (round oni)).year

        textX =
            doX model.chartScalings oni

        textY =
            doY model.chartScalings (deviations model -5.6 List.minimum)

        textPosition =
            Point2d.fromCoordinates ( textX, textY )

        mirrorAxis =
            Axis2d.through textPosition Direction2d.x

        tickText =
            Svg.text_
                [ fill "red"
                , x (String.fromFloat textX)
                , y (String.fromFloat textY)
                ]
                [ text (String.fromInt yearPart) ]
    in
    Svg.g []
        [ Svg.lineSegment2d
            [ Attributes.stroke "blue"
            , Attributes.strokeWidth "4"
            ]
            (LineSegment2d.fromEndpoints
                ( Point2d.fromCoordinates
                    ( doX model.chartScalings oni, doY model.chartScalings (deviations model lodev List.minimum) )
                , Point2d.fromCoordinates
                    ( doX model.chartScalings oni, doY model.chartScalings (deviations model -4.9 List.minimum) )
                )
            )
        , Svg.mirrorAcross mirrorAxis tickText
        ]


createMonthTicks : Model -> String -> Svg msg
createMonthTicks model ms =
    let
        timi =
            timify ms

        oni =
            toFloat timi

        monthPartNumber =
            -- add 1 hour to fix daylight saving time offset problems
            -- for the beginning of the month
            (ISO8601.fromTime (timi + (1000 * 3600 * 1))).month

        monthPart =
            monthNumName monthPartNumber

        textX =
            doX model.chartScalings oni

        textY =
            doY model.chartScalings (deviations model -5.2 List.minimum)

        textPosition =
            Point2d.fromCoordinates ( textX, textY )

        mirrorAxis =
            Axis2d.through textPosition Direction2d.x

        months =
            model.flags.axes.axis_x.month_starts

        reducedMonthPart =
            if List.length months > 30 then
                Just ""

            else if List.length months > 16 then
                monthQtrName monthPartNumber

            else
                monthPart

        tickText =
            Svg.text_
                [ fill "black"
                , x (String.fromFloat textX)
                , y (String.fromFloat textY)
                ]
                [ text (Maybe.withDefault "" reducedMonthPart) ]
    in
    Svg.g []
        [ Svg.lineSegment2d
            [ Attributes.stroke "black"
            , Attributes.strokeWidth "3"
            ]
            (LineSegment2d.fromEndpoints
                ( Point2d.fromCoordinates
                    ( doX model.chartScalings oni, doY model.chartScalings (deviations model lodev List.minimum) )
                , Point2d.fromCoordinates
                    ( doX model.chartScalings oni, doY model.chartScalings (deviations model -4.8 List.minimum) )
                )
            )
        , Svg.mirrorAcross mirrorAxis tickText
        ]


createWeekTicks : Model -> Int -> Svg msg
createWeekTicks model ws =
    let
        oni =
            toFloat ws
    in
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "1.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings (deviations model lodev List.minimum) )
            , Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings (deviations model -4.7 List.minimum) )
            )
        )


createDayTicks : Model -> Int -> Svg msg
createDayTicks model ds =
    let
        oni =
            toFloat ds
    in
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "1"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings (deviations model lodev List.minimum) )
            , Point2d.fromCoordinates
                ( doX model.chartScalings oni, doY model.chartScalings (deviations model -4.6 List.minimum) )
            )
        )


createMinorTick : Model -> Float -> Svg msg
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


weekTickVals : Model -> List Int
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


dayTickVals : Model -> List Int
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


createMajorTick : Model -> Float -> Svg msg
createMajorTick model mt =
    let
        ox =
            chartStart model.flags

        textX =
            doX model.chartScalings ox - 40

        textY =
            doY model.chartScalings mt

        textPosition =
            Point2d.fromCoordinates ( textX, textY )

        mirrorAxis =
            Axis2d.through textPosition Direction2d.x

        tickText =
            Svg.text_
                [ fill "black"
                , x (String.fromFloat textX)
                , y (String.fromFloat textY)
                ]
                [ text (String.fromFloat mt) ]
    in
    Svg.g []
        [ Svg.lineSegment2d
            [ Attributes.stroke "black"
            , Attributes.strokeWidth "1.5"
            ]
            (LineSegment2d.fromEndpoints
                ( Point2d.fromCoordinates
                    ( doX model.chartScalings ox, doY model.chartScalings mt )
                , Point2d.fromCoordinates
                    ( doX model.chartScalings ox - 10, doY model.chartScalings mt )
                )
            )

        -- we have to flip text manually
        , Svg.mirrorAcross mirrorAxis tickText
        ]


majorYticks : Model -> List Float
majorYticks model =
    let
        axis_y =
            model.flags.axes.axis_y

        upperBoundary =
            deviations model 6.7 List.maximum

        lowerBoundary =
            chartBottom model - axis_y.step

        all_ticks =
            findTicks axis_y.max lowerBoundary axis_y.step
    in
    List.filter (\t -> (t >= deviations model lodev List.minimum) && (t <= deviations model hidev List.maximum)) all_ticks


minorYticks : Model -> List Float
minorYticks model =
    let
        axis_y =
            model.flags.axes.axis_y

        upperBoundary =
            model.chartScalings.upperBoundary

        lowerBoundary =
            chartBottom model - axis_y.step

        scaling =
            -- reduce number of minor ticks if too many
            -- by increasing the distance between them
            if axis_y.step > 10 then
                5

            else
                1

        tickStep =
            axis_y.step / axis_y.step * scaling

        all_ticks =
            findTicks axis_y.max lowerBoundary tickStep
    in
    List.filter (\t -> (t >= deviations model lodev List.minimum) && (t <= deviations model hidev List.maximum)) all_ticks


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
