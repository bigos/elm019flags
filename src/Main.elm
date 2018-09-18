module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import ISO8601
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Triangle2d exposing (Triangle2d)
import Tuple



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
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
    , scaledPoints : List Point2d
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


type alias ChartRecord =
    { on : String
    , comment : String
    , by : String
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
    ( { flags = flags
      , level = 0
      , data = data
      , dateFrom = prepareTime flags.date_from
      , dateTo = prepareTime flags.date_to
      , chartType = "default"
      , points = points
      , chartBoundingBox = chartBoundingBox
      , scaledPoints = scaleXY flags points chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = None
    | NotApplicable


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- we do not do any update work at the moment
    ( model, Cmd.none )



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
            toFloat (timify flags.date_to - timify flags.date_from)

        distY =
            justValFn boundingBox BoundingBox2d.maxY - justValFn boundingBox BoundingBox2d.minY

        -- scale and offset
        scaleX =
            dx / distX

        offsetX =
            0 - justValFn boundingBox BoundingBox2d.minX

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



-- convert prescaled value to scaled one


doX : ChartScalings -> Float -> Float
doX cs x =
    cs.scaleX * (cs.offsetX + x)


doY : ChartScalings -> Float -> Float
doY cs y =
    cs.scaleY * (cs.offsetY + y)


scaleXY : Flags -> List Point2d -> Maybe BoundingBox2d -> List Point2d
scaleXY flags points boundingBox =
    let
        cs =
            setChartScalings flags boundingBox
    in
    List.map
        (\p ->
            Point2d.fromCoordinates
                ( doX cs (Point2d.xCoordinate p)
                , doY cs (Point2d.yCoordinate p)
                )
        )
        points


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


nominalLine : Model -> Svg msg
nominalLine model =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "0.5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.minX)
                , doY model.chartScalings model.flags.stats.nominal
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.maxX)
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
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.minX)
                , doY model.chartScalings model.flags.stats.mean
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.maxX)
                , doY model.chartScalings model.flags.stats.mean
                )
            )
        )


deviations : Model -> Float -> Float
deviations model x =
    model.flags.stats.mean - (model.flags.stats.deviation * x)


plusXdLine : Model -> Int -> Svg msg
plusXdLine model x =
    Svg.lineSegment2d
        [ Attributes.stroke "red"
        , Attributes.strokeWidth "0.4"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.minX)
                , doY model.chartScalings (deviations model (toFloat x))
                )
            , Point2d.fromCoordinates
                ( doX model.chartScalings (justValFn model.chartBoundingBox BoundingBox2d.maxX)
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


createShape : Point2d -> Svg msg
createShape point =
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        ]
        (Polygon2d.singleLoop (shape point))


shape : Point2d -> List Point2d
shape cc =
    -- draw tilted square around cc coordinates
    let
        scale =
            4.5

        cp =
            ( Point2d.xCoordinate cc, Point2d.yCoordinate cc )

        pcx =
            List.map
                (\p -> ( Tuple.first p * scale, Tuple.second p * scale ))
                -- possibility to refactor into creation of other shapes
                [ ( 0.0, 1.0 ), ( 1.0, 0.0 ), ( 0.0, -1.0 ), ( -1.0, 0.0 ) ]

        pc =
            List.map
                (\nc ->
                    ( Tuple.first cp - Tuple.first nc
                    , Tuple.second cp - Tuple.second nc
                    )
                )
                pcx
    in
    List.map
        (\c ->
            Point2d.fromCoordinates
                ( (Tuple.first cp + Tuple.first c) / 2.0
                , (Tuple.second cp + Tuple.second c) / 2.0
                )
        )
        pc


svgElements : Model -> List (Svg msg)
svgElements model =
    [ Svg.placeIn frameChart (nominalLine model)
    , Svg.placeIn frameChart (meanLine model)
    , Svg.placeIn frameChart (plusXdLine model 3)
    , Svg.placeIn frameChart (plusXdLine model 2)
    , Svg.placeIn frameChart (plusXdLine model -2)
    , Svg.placeIn frameChart (plusXdLine model -3)
    ]
        ++ List.map (\p -> Svg.placeIn frameChart (createShape p)) model.scaledPoints


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
                , viewBox "0 0 600 400"
                , style "border: solid red 1px;"
                ]
                [ Svg.g []
                    (svgElements model)
                ]
            ]
        , pdfLink model
        , div [ style "height:5em;" ] []
        ]
