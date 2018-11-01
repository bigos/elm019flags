module App.AnalyteSelector exposing (AnalyteSelector, initAnalyteSelector)


type alias AnalyteSelector =
    { machine : Maybe Int
    , sample : Maybe Int
    , analyte : Maybe Int
    , currentSelector : Maybe String
    , selectorOptions : List String
    , selectedItem : Maybe String
    , filter : Maybe String
    }


initAnalyteSelector =
    { machine = Nothing
    , sample = Nothing
    , analyte = Nothing
    , currentSelector = Nothing
    , selectorOptions = []
    , selectedItem = Nothing
    , filter = Nothing
    }
