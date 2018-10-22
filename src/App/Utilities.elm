module App.Utilities exposing (justTimeString, justValFn, monthNumName, monthQtrName, oneDay, timify, untimify)

import ISO8601
import List.Extra


timify : String -> Int
timify d =
    case ISO8601.fromString d of
        Ok nd ->
            ISO8601.toTime nd

        Err _ ->
            timify "1970-01-01T00:00:00Z"


untimify : Int -> String
untimify tint =
    let
        t =
            ISO8601.fromTime tint
    in
    ISO8601.toString t


justTimeString : Maybe ISO8601.Time -> String
justTimeString tv =
    case tv of
        Nothing ->
            ""

        Just tm ->
            ISO8601.toString tm


justValFn v fn =
    case v of
        Nothing ->
            0

        Just n ->
            fn n


monthNumName : Int -> Maybe String
monthNumName monthPartNumber =
    List.Extra.getAt
        (monthPartNumber - 1)
        [ "Jan" -- year is in place of Jan
        , "Feb"
        , "Mar"
        , "Apr"
        , "May"
        , "Jun"
        , "Jul"
        , "Aug"
        , "Sep"
        , "Oct"
        , "Nov"
        , "Dec"
        ]


monthQtrName : Int -> Maybe String
monthQtrName monthPartNumber =
    List.Extra.getAt
        (monthPartNumber - 1)
        [ "Jan" -- year is in place of Jan
        , ""
        , ""
        , "Apr"
        , ""
        , ""
        , "Jul"
        , ""
        , ""
        , "Oct"
        , ""
        , ""
        ]


oneDay : Int
oneDay =
    1000 * 3600 * 24
