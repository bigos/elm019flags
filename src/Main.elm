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



-- interesting bug TODO
-- http://localhost:3000/analytes/combined/dating_from/2016-12-21/dating_to/2017-12-22/analyte_ids/33920,%2020230,%2041843,%2038239,%2038305
-- inconsistently labels legend
-- consider cases where one of the analytes has zero data poins in the givcen
-- period, could the next one be showing in its place?
