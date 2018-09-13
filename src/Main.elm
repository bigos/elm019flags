module Main exposing (Flags, Model, Msg(..), init, main, update, view)

import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , level = 0
      }
    , Cmd.none
    )


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


type alias Model =
    { flags : Flags
    , level : Int
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


frameChart =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 100, 300 ))
        |> Frame2d.reverseY


frameAxisX =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 100, 350 ))
        |> Frame2d.reverseY


frameAxisY =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 50, 300 ))
        |> Frame2d.reverseY


frameLegend =
    Frame2d.atPoint
        (Point2d.fromCoordinates ( 300, 100 ))
        |> Frame2d.reverseY


placed =
    Svg.g []
        [ Svg.placeIn frameChart (stamp "yellow" rcc)
        , Svg.placeIn frameAxisX (stamp "blue" rcc)
        , Svg.placeIn frameAxisY (stamp "green" rcc)
        , Svg.placeIn frameLegend (stamp "red" rcc)
        ]


view model =
    div []
        [ div []
            [ Svg.svg
                [ width "420"
                , height "420"
                , viewBox "0 0 420 420"
                , style "border: solid red 1px;"
                ]
                [ placed ]
            ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.level) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text "We have lift off" ]
        ]
