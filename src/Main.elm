module Main exposing (ChartRecord, ChartScalings, Datum, Flags, Model, Msg(..), RawCid, ScaledPoint, Stats, Tooltip, TooltipData(..), axisX, axisY, chartEnd, chartStart, createMaintenanceLine, createMaintenanceShape, createQcShape, createReviewLine, createReviewShape, deviations, doX, doY, frameAxisX, frameAxisY, frameChart, frameLegend, genericShape, init, justTimeString, justValFn, main, maintenanceShape, meanLine, nominalLine, pdfLink, plusXdLine, prepareTime, readData, reviewShape, sampleml, scaleXY, setChartScalings, shape, subscriptions, svgElements, timify, toPoints, update, view)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, a, br, button, div, span, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as M exposing (..)
import ISO8601
import Json.Encode as E
import LineSegment2d exposing (LineSegment2d)
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


type alias Model =
    { chartBoundingBox : Maybe BoundingBox2d
    , chartScalings : ChartScalings
    , chartType : String
    , data : List Datum
    , dateFrom : Maybe ISO8601.Time
    , dateTo : Maybe ISO8601.Time
    , flags : Flags
    , level : Int
    , points : List Point2d
    , scaledPoints : List ScaledPoint
    , tooltip : Maybe Tooltip
    }


type alias Flags =
    { acqnominal : Float
    , analyteid : Int
    , chart_type : String
    , date_from : String
    , date_to : String
    , pdf : Bool
    , stats : Stats
    , maintenance_logs : List ChartRecord
    , reviews : List ChartRecord
    , qcresults : List RawCid
    }


type alias ChartScalings =
    { sizeX : Float
    , sizeY : Float
    , distX : Float
    , distY : Float
    , scaleX : Float
    , scaleY : Float
    , offsetX : Float
    , offsetY : Float
    }


type alias Datum =
    { time : Float
    , value : Float
    }


type alias Stats =
    { nominal : Float
    , mean : Float
    , deviation : Float
    }


type alias RawCid =
    { id : Int
    , c : Float
    , d : String
    }


type alias ScaledPoint =
    { point2d : Point2d
    , datum : Datum
    }


type alias ChartRecord =
    { on : String
    , by : String
    , comment : String
    }


type TooltipData
    = DataChartRecord ChartRecord
    | DataScaledPoint ScaledPoint


type alias Tooltip =
    { data : TooltipData
    , coordinates : ( Float, Float )
    , title : Maybe String
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        data =
            readData flags

        points =
            toPoints data

        chartBoundingBox =
            BoundingBox2d.containingPoints points
    in
    ( { chartBoundingBox = chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      , chartType = "default"
      , data = data
      , dateFrom = prepareTime flags.date_from
      , dateTo = prepareTime flags.date_to
      , flags = flags
      , level = 0
      , points = points
      , scaledPoints = scaleXY flags data chartBoundingBox
      , tooltip = Nothing
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = TooltipMouseEnter TooltipData ( Float, Float ) (Maybe String)
    | TooltipMouseLeave


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TooltipMouseEnter ttd cc title ->
            -- let
            --     tt = model.toolip
            --     tt.data = ttd
            --     tt.coordinates = cc
            --                      tt.title = title
            -- in
            ( { model | tooltip = Just (Tooltip ttd cc title) }
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


prepareTime : String -> Maybe ISO8601.Time
prepareTime s =
    case ISO8601.fromString s of
        Err msg ->
            Nothing

        Result.Ok d ->
            Just d


justValFn v fn =
    case v of
        Nothing ->
            0

        Just n ->
            fn n



-- OTHER FUNCTIONS


readData : Flags -> List Datum
readData flags =
    List.map (\d -> Datum (toFloat (timify d.d)) d.c) flags.qcresults


setChartScalings : Flags -> Maybe BoundingBox2d -> ChartScalings
setChartScalings flags boundingBox =
    let
        -- sizes x & y of the view area
        dx =
            500.0

        dy =
            200.0

        -- distances x & y before scaling
        distX =
            chartEnd flags - chartStart flags

        distY =
            justValFn boundingBox BoundingBox2d.maxY - justValFn boundingBox BoundingBox2d.minY

        -- scale and offset
        scaleX =
            dx / distX

        offsetX =
            0 - chartStart flags

        scaleY =
            dy / distY

        offsetY =
            0 - justValFn boundingBox BoundingBox2d.minY
    in
    { sizeX = dx
    , sizeY = dy
    , distX = distX
    , distY = distY
    , scaleX = scaleX
    , scaleY = scaleY
    , offsetX = offsetX
    , offsetY = offsetY
    }


chartStart : Flags -> Float
chartStart flags =
    toFloat (timify flags.date_from)


chartEnd : Flags -> Float
chartEnd flags =
    toFloat (timify flags.date_to)



-- convert prescaled value to scaled one


doX : ChartScalings -> Float -> Float
doX cs x =
    cs.scaleX * (cs.offsetX + x)


doY : ChartScalings -> Float -> Float
doY cs y =
    cs.scaleY * (cs.offsetY + y)


scaleXY flags data boundingBox =
    let
        cs =
            setChartScalings flags boundingBox

        points =
            toPoints data
    in
    List.map
        (\d ->
            { point2d = Point2d.fromCoordinates ( doX cs d.time, doY cs d.value )
            , datum = d
            }
        )
        data


toPoints : List Datum -> List Point2d
toPoints data =
    List.map (\d -> Point2d.fromCoordinates ( d.time, d.value )) data


timify : String -> Int
timify d =
    case ISO8601.fromString d of
        Ok nd ->
            ISO8601.toTime nd

        Err _ ->
            timify "1970-01-01T00:00:00Z"


axisX : Model -> Svg msg
axisX model =
    Svg.lineSegment2d
        [ Attributes.stroke "black"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( -50.0
                , doY model.chartScalings (deviations model -4.5)
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (chartEnd model.flags)
                , doY model.chartScalings (deviations model -4.5)
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
                , doY model.chartScalings (deviations model -5)
                )
            , Point2d.fromCoordinates
                ( 0.0
                , doY model.chartScalings (deviations model 5)
                )
            )
        )


nominalLine : Model -> Svg msg
nominalLine model =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings (chartStart model.flags)
                , doY model.chartScalings model.flags.stats.nominal
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (chartEnd model.flags)
                , doY model.chartScalings model.flags.stats.nominal
                )
            )
        )


meanLine : Model -> Svg msg
meanLine model =
    Svg.lineSegment2d
        [ Attributes.stroke "red"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings (chartStart model.flags)
                , doY model.chartScalings model.flags.stats.mean
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (chartEnd model.flags)
                , doY model.chartScalings model.flags.stats.mean
                )
            )
        )


deviations : Model -> Float -> Float
deviations model x =
    model.flags.stats.mean + (model.flags.stats.deviation * x)


plusXdLine : Model -> Int -> Svg msg
plusXdLine model x =
    Svg.lineSegment2d
        [ Attributes.stroke "red"
        , Attributes.strokeWidth "0.4"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings (chartStart model.flags)
                , doY model.chartScalings (deviations model (toFloat x))
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (chartEnd model.flags)
                , doY model.chartScalings (deviations model (toFloat x))
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


createQcShape : ScaledPoint -> Svg Msg
createQcShape point =
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter point event.pagePos Nothing)
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (shape point.point2d))


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


maintenanceShape point =
    genericShape point
        10.5
        [ ( 0.0, 1.0 ), ( 1.0, 0.0 ), ( 0.0, -1.0 ), ( -1.0, 0.0 ) ]


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


sampleml =
    { by = "Miss Sarah Cooke", comment = "Front end maintenance performed. Liner, gold seal, ferrule and o-ring all changed. Still a slight air leak of 25%. Trasnfer line to be checked as soon as the instrument is available to vent.", on = "2018-06-26" }


createMaintenanceLine model ml =
    let
        oni =
            toFloat (timify ml.on)

        ld =
            deviations model -4

        ud =
            deviations model 5
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


createReviewLine model ml =
    let
        oni =
            toFloat (timify ml.on)

        ld =
            deviations model -4

        ud =
            deviations model 4.5
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


createMaintenanceShape model ml =
    let
        oni =
            toFloat (timify ml.on)

        ud =
            deviations model 5

        point =
            Point2d.fromCoordinates ( doX model.chartScalings oni, doY model.chartScalings ud )
    in
    Svg.polygon2d
        [ Attributes.fill "red"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter ml event.pagePos (Just "Maintnance"))
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (maintenanceShape point))


createReviewShape model r =
    let
        oni =
            toFloat (timify r.on)

        ud =
            deviations model 4.5

        point =
            Point2d.fromCoordinates ( doX model.chartScalings oni, doY model.chartScalings ud )
    in
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        , M.onEnter (\event -> TooltipMouseEnter r event.pagePos (Just "Review"))
        , M.onLeave (\event -> TooltipMouseLeave)
        ]
        (Polygon2d.singleLoop (reviewShape point))



--svgElements : Model -> List (Svg msg)


svgElements model =
    [ Svg.placeIn frameChart (axisX model)
    , Svg.placeIn frameChart (axisY model)
    , Svg.placeIn frameChart (nominalLine model)
    , Svg.placeIn frameChart (meanLine model)
    , Svg.placeIn frameChart (plusXdLine model 3)
    , Svg.placeIn frameChart (plusXdLine model 2)
    , Svg.placeIn frameChart (plusXdLine model -2)
    , Svg.placeIn frameChart (plusXdLine model -3)
    ]
        ++ List.map (\p -> Svg.placeIn frameChart (createQcShape p)) model.scaledPoints
        ++ List.map (\ml -> Svg.placeIn frameChart (createMaintenanceLine model ml)) model.flags.maintenance_logs
        ++ List.map (\ml -> Svg.placeIn frameChart (createMaintenanceShape model ml)) model.flags.maintenance_logs
        ++ List.map (\r -> Svg.placeIn frameChart (createReviewLine model r)) model.flags.reviews
        ++ List.map (\r -> Svg.placeIn frameChart (createReviewShape model r)) model.flags.reviews


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


view : Model -> Html Msg
view model =
    div []
        [ div [ style "margin: auto ; width:600px" ]
            [ Svg.svg
                [ height "400"
                , viewBox "0 0 700 400"
                , style "border: solid red 1px;"
                ]
                [ Svg.g []
                    (svgElements model)
                ]
            ]

        -- , showTheTooltip2 model
        -- , showTheTooltip1 model
        , div [] [ text (Debug.toString model.tooltip) ]
        , pdfLink model
        , div [ style "height:5em;" ] []
        ]



-- showTheTooltip1 model =
--     case model.recordTooltip of
--         Nothing ->
--             div [] []
--         Just tt ->
--             div
--                 [ class "log-record-tooltip"
--                 , style
--                     ("left: "
--                         ++ String.fromFloat (10 + Tuple.first model.coordinates)
--                         ++ "px;"
--                         ++ "top: "
--                         ++ String.fromFloat (10 + Tuple.second model.coordinates)
--                         ++ "px;"
--                     )
--                 ]
--                 [ span [ class "tool-tip-title" ] [ text "Manintenance Log on: " ]
--                 , span [] [ text tt.on ]
--                 , br [] []
--                 , span [ class "tool-tip-title" ] [ text "By: " ]
--                 , span [] [ text tt.by ]
--                 , br [] []
--                 , span [ class "tool-tip-title" ] [ text "Comment: " ]
--                 , span [] [ text tt.comment ]
--                 ]
-- showTheTooltip2 model =
--     case model.qcTooltip of
--         Nothing ->
--             div [] []
--         Just t ->
--             let
--                 tm =
--                     ISO8601.fromTime (floor t.time)
--                 t2 =
--                     ISO8601.toString tm
--                 t3 =
--                     List.head (String.split "Z" t2)
--                 t4 =
--                     String.split "T" (Maybe.withDefault "" t3)
--                 dx =
--                     List.head t4
--                 tx =
--                     case List.tail t4 of
--                         Nothing ->
--                             ""
--                         Just s ->
--                             Maybe.withDefault "" (List.head s)
--             in
--             div []
--                 [ span
--                     [ class "tool-tip-title" ]
--                     [ text "Date: " ]
--                 , span [] [ text (Maybe.withDefault "" dx) ]
--                 , br [] []
--                 , span [ class "tool-tip-title" ] [ text "Time: " ]
--                 , span [] [ text tx ]
--                 , br [] []
--                 , span [ class "tool-tip-title" ]
--                     [ text "Concentration: " ]
--                 , span
--                     []
--                     [ text (String.fromFloat t.value) ]
--                 ]
