module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import ISO8601
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Triangle2d exposing (Triangle2d)
import Tuple


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { flags : Flags
    , level : Int
    , data : List Datum
    , points : List Point2d
    , chartBoundingBox : Maybe BoundingBox2d
    , chartScalings : ChartScalings
    , scaledPoints : List Point2d
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
      , points = points
      , chartBoundingBox = chartBoundingBox
      , scaledPoints = scaleXY flags points chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      }
    , Cmd.none
    )


justValFn v fn =
    case v of
        Nothing ->
            0

        Just n ->
            fn n


setChartScalings flags boundingBox =
    let
        -- sizes x & y of the view area
        dx =
            250.0

        dy =
            100.0

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


doX cs x =
    cs.scaleX * (cs.offsetX + x)


doY cs y =
    cs.scaleY * (cs.offsetY + y)


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


toPoints data =
    List.map (\d -> Point2d.fromCoordinates ( d.time, d.value )) data


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


type alias RawCid =
    { id : Int
    , c : Float
    , d : String
    }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            ( { model | level = model.level + 1 }, Cmd.none )

        Decrement ->
            ( { model | level = model.level - 1 }, Cmd.none )


timify d =
    case ISO8601.fromString d of
        Ok nd ->
            ISO8601.toTime nd

        Err _ ->
            timify "1970-01-01T00:00:00Z"


triangle : Svg Msg
triangle =
    Svg.triangle2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "orange"
        ]
        (Triangle2d.fromVertices
            ( Point2d.fromCoordinates ( 0, 0 )
            , Point2d.fromCoordinates ( 60, 5 )
            , Point2d.fromCoordinates ( 5, 60 )
            )
        )


vertices =
    [ Point2d.fromCoordinates ( 0, 0 )
    , Point2d.fromCoordinates ( 100, 0 )
    , Point2d.fromCoordinates ( 0, 100 )
    ]


rcc =
    [ ( 0, 0 ), ( 50, 0 ), ( 50, 50 ), ( 0, 50 ) ]


rect cc =
    List.map (\c -> Point2d.fromCoordinates c) cc


stamp col cc =
    Svg.polygon2d
        [ Attributes.fill col
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Polygon2d.singleLoop (rect cc))


nominalLine model =
    Svg.lineSegment2d
        [ Attributes.stroke "grey"
        , Attributes.strokeWidth "2"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates
                ( 0
                , 0
                )
            , Point2d.fromCoordinates
                ( 250
                , 100
                )
            )
        )


stamp2 col cc =
    Svg.polygon2d
        [ Attributes.fill col
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.5"
        ]
        (Polygon2d.singleLoop cc)


frameChart =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 75, 300 ))
        |> Frame2d.reverseY


frameAxisX =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 75, 350 ))
        |> Frame2d.reverseY


frameAxisY =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 25, 300 ))
        |> Frame2d.reverseY


frameLegend =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 500, 100 ))
        |> Frame2d.reverseY


dat : Point2d -> Svg msg
dat point =
    Svg.polygon2d
        [ Attributes.fill "blue"
        , Attributes.stroke "black"
        , Attributes.strokeWidth "0.25"
        ]
        (Polygon2d.singleLoop (shape point))


shape cc =
    -- draw tilted square around cc coordinates
    -- possibility to refactor into creation of other shapes
    let
        factor =
            3.1

        cp =
            ( Point2d.xCoordinate cc, Point2d.yCoordinate cc )

        pcx =
            List.map
                (\p -> ( Tuple.first p * factor, Tuple.second p * factor ))
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
                ( Tuple.first cp + Tuple.first c
                , Tuple.second cp + Tuple.second c
                )
        )
        pc


svgElements : Model -> List (Svg msg)
svgElements model =
    [ Svg.placeIn frameChart (stamp "yellow" rcc)
    , Svg.placeIn frameAxisX (stamp "blue" rcc)
    , Svg.placeIn frameAxisY (stamp "green" rcc)
    , Svg.placeIn frameLegend (stamp "red" rcc)
    , Svg.placeIn frameChart (nominalLine model)
    ]
        ++ List.map (\p -> Svg.placeIn frameChart (dat p)) model.scaledPoints


placed model =
    Svg.g []
        (svgElements model)


readData flags =
    List.map (\d -> Datum (toFloat (timify d.d)) d.c) flags.qcresults


view model =
    div []
        [ div [ style "margin: auto ; width:600px" ]
            [ Svg.svg
                [ height "400"
                , viewBox "0 0 600 400"
                , style "border: solid red 1px;"
                ]
                [ placed model ]
            ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.level) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text "We have lift off" ]
        , div [] [ text (Debug.toString model.data) ]
        , div [ style "height:5em;" ] []
        ]
