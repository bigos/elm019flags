module Main exposing (Flags, Msg(..), main, update, view)

import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
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
    let
        points =
            List.map (\d -> Point2d.fromCoordinates ( toFloat d.t, toFloat d.v )) flags.data
    in
    ( { flags = flags
      , level = 0
      , pointData = points
      , scaledPoints = scaleXY points
      }
    , Cmd.none
    )



-- we need to properly calculate scale and offset from bounding box
-- https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/BoundingBox2d


scaleXY points =
    let
        sx =
            0.5

        ox =
            10.0

        sy =
            0.25

        oy =
            20.0
    in
    List.map
        (\p ->
            Point2d.fromCoordinates
                ( ox + sx * Point2d.xCoordinate p
                , oy + sy * Point2d.yCoordinate p
                )
        )
        points


type alias Model =
    { flags : Flags
    , level : Int
    , pointData : List Point2d
    , scaledPoints : List Point2d
    }


type Msg
    = Increment
    | Decrement


type alias Flags =
    { num : Int
    , str : String
    , data : List Datum
    }


type alias Datum =
    { t : Int, v : Int }


update msg model =
    case msg of
        Increment ->
            ( { model | level = model.level + 1 }, Cmd.none )

        Decrement ->
            ( { model | level = model.level - 1 }, Cmd.none )


vertices =
    [ Point2d.fromCoordinates ( 5, 5 )
    , Point2d.fromCoordinates ( 20, 5 )
    , Point2d.fromCoordinates ( 5, 20 )
    ]


stamp =
    Svg.polygon2d
        [ Attributes.fill "orange"
        , Attributes.stroke "blue"
        , Attributes.strokeWidth "2"
        ]
        (Polygon2d.singleLoop vertices)


frames =
    [ Frame2d.atPoint
        (Point2d.fromCoordinates ( 25, 25 ))
    , Frame2d.atPoint
        (Point2d.fromCoordinates ( 100, 25 ))
        |> Frame2d.reverseY
    , Frame2d.atPoint
        (Point2d.fromCoordinates ( 175, 25 ))
        |> Frame2d.rotateBy (degrees 20)
    , Frame2d.atPoint
        (Point2d.fromCoordinates ( 25, 150 ))
    , Frame2d.atPoint
        (Point2d.fromCoordinates ( 100, 100 ))
        |> Frame2d.rotateBy (degrees 20)
    , Frame2d.atPoint
        (Point2d.fromCoordinates ( 150, 150 ))
        |> Frame2d.rotateBy (degrees -30)
    ]


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.level) ]
        , button [ onClick Increment ] [ text "+" ]
        , div []
            [ Svg.svg [ Attributes.viewBox "0 0 600 600" ]
                [ Svg.g
                    []
                    (frames
                        |> List.map
                            (\frame -> Svg.placeIn frame stamp)
                    )
                ]
            ]
        ]
