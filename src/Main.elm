module Main exposing (main)

import App.Model exposing (..)
import App.Subscriptions exposing (..)
import App.Update exposing (..)
import App.Utilities exposing (..)
import App.View exposing (view)
import Browser


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
