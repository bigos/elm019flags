-- code refactoring inspired by
-- https://github.com/halfzebra/elm-examples/blob/master/examples/fractal-architecture/src/Main.elm
-- Notes for tomorrow
-- We can compare the diff between branches wip7 and stripped
-- to see the removed code
-- that will give us a chance to start restoring the code and at the same time
-- taking into consideration the differences related to chart stats chunks


module Main exposing (main, subscriptions, update)

import App.Model exposing (..)
import App.Utilities exposing (..)
import App.View exposing (..)
import Browser
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as M exposing (..)
import ISO8601
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes exposing (..)
import Svg.Events as Events exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



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



-- convert prescaled value to scaled one
