module App.AnalyteSelector exposing (AnalyteSelector, initAnalyteSelector, machineList)

import Selectize


type alias AnalyteSelector =
    { machineSelection : Maybe String
    , machineMenu : Selectize.State String
    }


initAnalyteSelector =
    { machineSelection = Nothing
    , machineMenu = Selectize.closed "machine-menu" identity machineList
    }


machineList =
    let
        mm =
            [ "mach1", "mach2" ]
    in
    List.map Selectize.entry mm
