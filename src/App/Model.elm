module App.Model exposing (AdditionStage(..), Analyte, AnalyteResults, AxisData, AxisX, AxisY, ChartRecord, ChartScalings, Datum, Flags, Machine, Model, Msg(..), RawCid, Sample, ScaledPoint, StatsData, Tooltip, TooltipData(..), Tree, averageMean, chartBottom, chartEnd, chartStart, chartTop, defaultAnalyteData, deviations, doX, doY, findStatForTime, flatten, hidev, init, largestDeviation, lodev, prepareTime, readCombinedData, scaleXY, setChartScalings, singleAnalyteId, singleResults, standardDeviation, statStartTimes, statStartTuples, tickBottom, toPoints, tupleize, tupleizeHelper)

import App.Utilities exposing (..)
import BoundingBox2d exposing (BoundingBox2d)
import Http exposing (..)
import ISO8601
import List.Extra
import Point2d exposing (Point2d)
import Selectize


type alias Model =
    { chartBoundingBox : Maybe BoundingBox2d
    , chartScalings : ChartScalings
    , chartType : String
    , data : List (List Datum)
    , dateFrom : Maybe ISO8601.Time
    , dateTo : Maybe ISO8601.Time
    , flags : Flags
    , points : List (List Point2d)
    , scaledPoints : List (List ScaledPoint)
    , tooltip : Maybe Tooltip
    , textfieldSelection : Maybe Tree
    , textfieldMenu : Selectize.State Tree
    , textfieldMenuOptions : Maybe (List String)
    , textfieldMenuPlaceholder : String
    , combinedAdditionStage : Maybe AdditionStage
    , combinedAdditionMachine : Maybe Int
    , combinedAdditionSample : Maybe Int
    , combinedAdditionAnalyte : Maybe Int
    }


type alias Tree =
    { id : String
    , name : String
    }


type AdditionStage
    = StageMachine
    | StageSample
    | StageAnalyte
    | StageAnalyteConfirmation


type alias Flags =
    { analytes :
        List
            { id : Int
            , analyte : String
            , sample : String
            , machine : String
            , eid : Int
            }
    , chart_type : String
    , date_from : String
    , date_to : String
    , show_pdf_download : Bool
    , axes : AxisData
    , stats : List StatsData
    , maintenance_logs : List ChartRecord
    , reviews : List ChartRecord
    , qcresults : List AnalyteResults
    }


type alias AxisData =
    { axis_x : AxisX, axis_y : AxisY }


type alias AxisX =
    { days : Int
    , weeks : Int
    , months : Int
    , years : Int
    , month_starts : List String
    , year_starts : List String
    , monday : String
    }


type alias AxisY =
    { min : Float
    , max : Float
    , ticks : Int
    , step : Float
    }


type alias ChartScalings =
    { sizeX : Float
    , sizeY : Float
    , distX : Float
    , distY : Float
    , scaleX : Float
    , scaleY : Float
    , offsetX : Float
    , offsetY : Float
    , upperBoundary : Float
    , lowerBoundary : Float
    }


type alias Datum =
    { time : Float
    , value : Float
    , aid : Int
    }


type alias StatsData =
    { start_date : String
    , deviation : Float
    , mean : Float
    , nominal : Float
    }


type alias AnalyteResults =
    List RawCid


type alias RawCid =
    { id : Int
    , c : Float
    , d : String
    , aid : Maybe Int
    }


type alias ScaledPoint =
    { point2d : Point2d
    , datum : Datum
    }


type alias ChartRecord =
    { on : String
    , by : String
    , comment : String
    }


type TooltipData
    = DataChartRecord ChartRecord
    | DataScaledPoint ScaledPoint
    | DataCombinedPoint ScaledPoint


type alias Tooltip =
    { data : TooltipData
    , coordinates : ( Float, Float )
    , title : Maybe String
    }



-- API types


type alias Machine =
    { eid : Int
    , name : String
    }


type alias Sample =
    { sampleid : Int
    , name : String
    }


type alias Analyte =
    { analyteid : Int
    , name : String
    }



-- UPDATE TYPES


type Msg
    = TooltipMouseEnter TooltipData ( Float, Float ) (Maybe String)
    | TooltipMouseLeave
    | GetMachines
    | RequestedMachines (Result Http.Error (List Machine))
    | RequestedSamples (Result Http.Error (List Sample))
    | RequestedAnalytes (Result Http.Error (List Analyte))
    | TextfieldMenuMsg (Selectize.Msg Tree)
    | SelectTextfieldOption (Maybe Tree)



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        data =
            readCombinedData flags

        points =
            toPoints data

        flattenedPoints =
            List.concatMap identity points

        chartBoundingBox =
            BoundingBox2d.containingPoints flattenedPoints
    in
    ( { chartBoundingBox = chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      , chartType = flags.chart_type
      , data = data
      , dateFrom = prepareTime flags.date_from
      , dateTo = prepareTime flags.date_to
      , flags = flags
      , points = points
      , scaledPoints = scaleXY flags data chartBoundingBox
      , tooltip = Nothing
      , textfieldSelection = Nothing
      , textfieldMenu =
            Selectize.closed
                "textfield-menu"
                (\tree -> tree.id ++ " - " ++ tree.name)
                []
      , textfieldMenuOptions = Nothing
      , textfieldMenuPlaceholder = "Waiting for Command"
      , combinedAdditionStage = Nothing
      , combinedAdditionMachine = Nothing
      , combinedAdditionSample = Nothing
      , combinedAdditionAnalyte = Nothing
      }
    , Cmd.none
    )


findStatForTime : List StatsData -> Int -> Maybe StatsData
findStatForTime stats time =
    let
        possibleStats =
            List.Extra.takeWhile (\s -> timify s.start_date <= time) stats

        res =
            List.Extra.last possibleStats
    in
    res


tupleize : List a -> List ( a, Maybe a )
tupleize xs =
    tupleizeHelper xs [] |> List.reverse


tupleizeHelper : List a -> List ( a, Maybe a ) -> List ( a, Maybe a )
tupleizeHelper xs acc =
    case xs of
        x :: y :: rest ->
            tupleizeHelper (y :: rest) (( x, Just y ) :: acc)

        [ x ] ->
            ( x, Nothing ) :: acc

        [] ->
            acc


statStartTimes : Model -> List Int
statStartTimes model =
    List.map (\s -> timify s.start_date) model.flags.stats


statStartTuples model =
    Debug.log ("debugging start tuples with " ++ Debug.toString (statStartTimes model))
        (tupleize (statStartTimes model))


flatten lst =
    List.concatMap identity lst


singleResults : Flags -> AnalyteResults
singleResults flags =
    flatten flags.qcresults


singleAnalyteId : Model -> Int
singleAnalyteId model =
    let
        anh =
            List.head
                model.flags.analytes

        an =
            Maybe.withDefault
                defaultAnalyteData
                anh
    in
    an.id


defaultAnalyteData =
    { id = 0
    , analyte = ""
    , sample = ""
    , machine = ""
    , eid = 0
    }


readCombinedData : Flags -> List (List Datum)
readCombinedData flags =
    let
        combinedData =
            flags.qcresults
    in
    List.map
        (\singleDataList ->
            List.map
                (\d ->
                    Datum
                        (toFloat (timify d.d))
                        d.c
                        (Maybe.withDefault
                            0
                            d.aid
                        )
                )
                singleDataList
        )
        combinedData


toPoints : List (List Datum) -> List (List Point2d)
toPoints combinedData =
    List.map
        (\data ->
            List.map (\d -> Point2d.fromCoordinates ( d.time, d.value )) data
        )
        combinedData


prepareTime : String -> Maybe ISO8601.Time
prepareTime s =
    case ISO8601.fromString s of
        Err msg ->
            Nothing

        Result.Ok d ->
            Just d


scaleXY : Flags -> List (List Datum) -> Maybe BoundingBox2d -> List (List ScaledPoint)
scaleXY flags combinedData boundingBox =
    let
        cs =
            setChartScalings flags boundingBox

        points =
            toPoints combinedData
    in
    List.map
        (\data ->
            List.map
                (\d ->
                    { point2d = Point2d.fromCoordinates ( doX cs d.time, doY cs d.value )
                    , datum = d
                    }
                )
                data
        )
        combinedData


doX : ChartScalings -> Float -> Float
doX cs x =
    cs.scaleX * (cs.offsetX + x)


doY : ChartScalings -> Float -> Float
doY cs y =
    cs.scaleY * (cs.offsetY + y)


chartStart : Flags -> Float
chartStart flags =
    toFloat (timify flags.date_from)


chartEnd : Flags -> Float
chartEnd flags =
    toFloat (timify flags.date_to + oneDay)



-- dataStats : Model -> Bool
-- dataStats model =
--     let
--         qcr =
--             List.map (\qc -> qc.c) (singleResults model.flags)
--         values =
--             List.map (\qr -> qr.c) (singleResults model.flags)
--         mean =
--             List.foldl (+) 0.0 values / toFloat (List.length values)
--     in
--     1


deviations model x fn =
    let
        stats =
            model.flags.stats

        stats2 =
            if List.length stats == 0 then
                let
                    values =
                        List.map (\qr -> qr.c) (singleResults model.flags)

                    mean =
                        List.foldl (+) 0.0 values / toFloat (List.length values)
                in
                if List.length values == 0 then
                    [ { start_date = ""
                      , deviation = 25.0
                      , mean = 250.0
                      , nominal = 250.0
                      }
                    ]

                else
                    [ { start_date = ""
                      , deviation = standardDeviation model.flags
                      , mean = mean
                      , nominal = mean
                      }
                    ]

            else
                stats

        devs =
            List.map (\s -> s.mean + s.deviation * x) stats2

        calc =
            fn devs
    in
    Maybe.withDefault 0.0 calc


hidev =
    4.5


lodev =
    -4.5


chartBottom : Model -> Float
chartBottom model =
    doY model.chartScalings (deviations model lodev List.minimum)


chartTop : Model -> Float
chartTop model =
    doY model.chartScalings (deviations model hidev List.maximum)



-- change me taking into consideration average mean and largest deviation


tickBottom model =
    doY model.chartScalings (deviations model -4.7 List.minimum)


averageMean : Flags -> Float
averageMean flags =
    let
        meanValues =
            List.map (\s -> s.mean) flags.stats
    in
    if List.length meanValues == 0 then
        let
            qcvalues =
                singleResults flags
        in
        if List.length qcvalues == 0 then
            250.0

        else
            List.foldl (+) 0.0 (List.map (\qc -> qc.c) qcvalues)
                / toFloat (List.length qcvalues)

    else
        List.foldl (+) 0.0 meanValues / toFloat (List.length meanValues)


standardDeviation flags =
    let
        values =
            List.map (\qc -> qc.c) (singleResults flags)

        mean =
            List.foldl (+) 0.0 values / toFloat (List.length values)

        sqrd =
            List.foldl (\v a -> a + ((v - mean) ^ 2)) 0.0 values
    in
    if List.length values == 0 then
        25.0

    else
        sqrt (sqrd / toFloat (List.length values - 1))


largestDeviation : Flags -> Float
largestDeviation flags =
    let
        deviationValues =
            List.map (\s -> s.deviation) flags.stats
    in
    if List.length flags.stats == 0 then
        standardDeviation flags

    else if List.length deviationValues == 0 then
        standardDeviation flags

    else
        Maybe.withDefault 35.0 (List.maximum deviationValues)


setChartScalings : Flags -> Maybe BoundingBox2d -> ChartScalings
setChartScalings flags boundingBox =
    let
        mean =
            averageMean flags

        deviation =
            largestDeviation flags

        scalingFactor =
            -- greater number = smaller chart
            2.9

        upperBoundary =
            mean + deviation * scalingFactor

        lowerBoundary =
            mean - deviation * scalingFactor

        -- sizes x & y of the view area
        dx =
            500.0

        dy =
            200.0

        -- distances x & y before scaling
        distX =
            chartEnd flags - chartStart flags

        distY =
            upperBoundary - lowerBoundary

        -- scale and offset
        scaleX =
            dx / distX

        offsetX =
            0 - chartStart flags

        scaleY =
            dy / distY

        offsetY =
            0 - lowerBoundary
    in
    { sizeX = dx
    , sizeY = dy
    , distX = distX
    , distY = distY
    , scaleX = scaleX
    , scaleY = scaleY
    , offsetX = offsetX
    , offsetY = offsetY
    , upperBoundary = upperBoundary
    , lowerBoundary = lowerBoundary
    }
