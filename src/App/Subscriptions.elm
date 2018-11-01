module App.Subscriptions exposing (subscriptions)

import App.Model exposing (..)
import Browser.Events


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown Keypress
        ]
