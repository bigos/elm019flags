module App.Subscriptions exposing (subscriptions)

import App.Model exposing (..)
import Browser.Events
import Json.Decode as Decode


keyDecoder =
    Decode.map toKeypress (Decode.field "key" Decode.string)


toKeypress string =
    Keypress string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        ]
