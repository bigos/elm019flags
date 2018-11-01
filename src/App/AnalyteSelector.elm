module App.AnalyteSelector exposing (AnalyteSelector, SelectorSelection(..), initAnalyteSelector, setMachineOptions)


type alias AnalyteSelector =
    { machine : Maybe Int
    , sample : Maybe Int
    , analyte : Maybe Int
    , currentSelector : SelectorSelection
    , selectorOptions : List String
    , selectedItem : Maybe String
    , filter : Maybe String
    }


type SelectorSelection
    = MachineSelection
    | SampleSelection
    | AnalyteSelection


initAnalyteSelector =
    { machine = Nothing
    , sample = Nothing
    , analyte = Nothing
    , currentSelector = MachineSelection
    , selectorOptions = []
    , selectedItem = Nothing
    , filter = Nothing
    }


setMachineOptions ans opts =
    { machine = Nothing
    , sample = Nothing
    , analyte = Nothing
    , currentSelector = MachineSelection
    , selectorOptions = opts
    , selectedItem = Nothing
    , filter = Nothing
    }
