module App.Model exposing (AdditionStage(..), Analyte, AnalyteRecord, AnalyteResults, AxisData, AxisX, AxisY, ChartRecord, ChartScalings, ClassifiedSection, DataStats, Datum, Flags, LegendElement, LegendShape(..), Machine, Model, Msg(..), RawCid, Sample, ScaledPoint, SectionData, StatsData, Tooltip, TooltipData(..), Tree, analyteFullName, averageMean, chartBottom, chartEnd, chartStart, chartTop, dataPointColours, dataStats, defaultAnalyteData, deviations, doX, doY, findStatForTime, fixedFlagCheck, flatten, hidev, init, largestDeviation, legendData, lodev, prepareTime, readCombinedData, readValidData, scaleXY, setChartScalings, singleAnalyteId, singleValidResults, standardDeviation, statStartTimes, statStartTuples, tickBottom, toPoints, tupleize, tupleizeHelper)

import App.Utilities exposing (..)
import BoundingBox2d exposing (BoundingBox2d)
import Http exposing (..)
import ISO8601
import List.Extra
import Point2d exposing (Point2d)
import Selectize
import String.Interpolate exposing (interpolate)



{-
   good example of a problem with the existing approach
   http://localhost:3000/analytes/combined/dating_from/2013-06-26/dating_to/2013-07-17/analyte_ids/19682,19552,19551,19550

-}
-- we have a problem with bounding box calculation that results in x coordinate being way off the chart


type alias Model =
    { chartBoundingBox : Maybe BoundingBox2d
    , chartScalings : ChartScalings
    , chartType : String
    , dateFrom : Maybe ISO8601.Time
    , dateTo : Maybe ISO8601.Time
    , flags : Flags
    , scaledAbovePoints : List (List ScaledPoint)
    , scaledValidPoints : List (List ScaledPoint)
    , scaledBelowPoints : List (List ScaledPoint)
    , tooltip : Maybe Tooltip
    , textfieldSelection : Maybe Tree
    , textfieldMenu : Selectize.State Tree
    , textfieldMenuOptions : Maybe (List String)
    , textfieldMenuPlaceholder : String
    , combinedAdditionStage : Maybe AdditionStage
    , combinedAdditionMachine : Maybe Int
    , combinedAdditionSample : Maybe Int
    , combinedAdditionAnalyte : Maybe Int
    , legend : List LegendElement
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
    { analytes : List AnalyteRecord
    , chart_type : String
    , host : String
    , date_from : String
    , date_to : String
    , show_pdf_download : Bool
    , axes : AxisData
    , stats : List StatsData
    , maintenance_logs : List ChartRecord
    , reviews : List ChartRecord
    , boundaries : { above : Float, below : Float, bottom : Float }
    , classified_qcresults : List ClassifiedSection
    }


type alias ClassifiedSection =
    { stats : String
    , above_invalid : Float
    , below_invalid : Float
    , chart_bottom : Float
    , values :
        { above_valid : List RawCid
        , valid : List RawCid
        , below_valid : List RawCid
        }
    }


type ValuesClassification
    = ValuesValid
    | ValuesAbove
    | ValuesBelow


type alias AnalyteRecord =
    { id : Int
    , analyte : String
    , sample : String
    , machine : String
    , eid : Int
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


type alias DataStats =
    { mean : Float, sd : Float }


type alias SectionData =
    { start : Int, fixed : Bool }


type alias StatsData =
    { start_date : String
    , deviation : Float
    , mean : Float
    , nominal : Float
    , min : Maybe Float
    , max : Maybe Float
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


type alias LegendElement =
    { shape : LegendShape, description : String }


type LegendShape
    = LegendDataPoint (List String) (List AnalyteRecord)
    | LegendMaintenanceLog
    | LegendChartReview
    | LegendTheoreticalLine
    | LegendLimitRed
    | LegendLimitOrange
    | LegendOutsideValid


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dataValid =
            readValidData flags

        dataAbove =
            readInvalidData flags "above"

        dataBelow =
            readInvalidData flags "below"

        flattenedPoints =
            List.concatMap identity (toPoints dataValid)

        chartBoundingBox =
            BoundingBox2d.containingPoints flattenedPoints

        hasInvalid =
            not (List.empty dataAbove && List.empty dataBelow)
    in
    ( { chartBoundingBox = chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      , chartType = flags.chart_type
      , dateFrom = prepareTime flags.date_from
      , dateTo = prepareTime flags.date_to
      , flags = flags
      , scaledAbovePoints = scaleXY flags dataAbove chartBoundingBox ValuesAbove
      , scaledValidPoints = scaleXY flags dataValid chartBoundingBox ValuesValid
      , scaledBelowPoints = scaleXY flags dataBelow chartBoundingBox ValuesBelow
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
      , legend = legendData flags [] (LegendDataPoint dataPointColours flags.analytes hasInvalid) dataAbove dataBelow
      }
    , Cmd.none
    )


dataPointColours : List String
dataPointColours =
    [ "blue", "red", "black", "green", "yellow", "pink" ]


analyteFullName : AnalyteRecord -> String
analyteFullName analyte =
    interpolate "{0} - {1} - {2} - EID: {3}"
        [ analyte.analyte
        , analyte.sample
        , analyte.machine
        , String.fromInt analyte.eid
        ]


legendData flags acc nextShape =
    case nextShape of
        LegendDataPoint colours validAnalytes hasInvalid ->
            if List.length validAnalytes > 1 then
                legendData flags
                    (LegendElement (LegendDataPoint (List.take 1 colours) (List.take 1 validAnalytes))
                        (analyteFullName
                            (Maybe.withDefault (AnalyteRecord 0 "error" "" "" 0) (List.head validAnalytes))
                        )
                        :: acc
                    )
                    (LegendDataPoint (Maybe.withDefault [] (List.tail colours))
                        (Maybe.withDefault [] (List.tail validAnalytes))
                    )

            else
                legendData flags
                    (LegendElement
                        (LegendDataPoint (List.take 1 colours)
                            (List.take 1 validAnalytes)
                        )
                        (analyteFullName
                            (Maybe.withDefault (AnalyteRecord 0 "error" "" "" 0) (List.head validAnalytes))
                        )
                        :: acc
                    )
                    (LegendOutsideValid hasInvalid)

        LegendOutsideValid hasInvalid ->
            let
                newAcc =
                    if not hasInvalid then
                        acc

                    else
                        LegendElement LegendOutsideValid "Results outside valid range"
                            :: acc
            in
            legendData flags newAcc LegendMaintenanceLog

        LegendMaintenanceLog ->
            let
                newAcc =
                    if List.isEmpty flags.maintenance_logs then
                        acc

                    else
                        LegendElement LegendMaintenanceLog "Maintenance log"
                            :: acc
            in
            legendData flags newAcc LegendChartReview

        LegendChartReview ->
            let
                newAcc =
                    if List.isEmpty flags.reviews then
                        acc

                    else
                        LegendElement LegendChartReview "Chart review"
                            :: acc
            in
            legendData flags newAcc LegendTheoreticalLine

        LegendTheoreticalLine ->
            let
                newAcc =
                    if flags.chart_type == "combined" then
                        acc

                    else
                        LegendElement LegendTheoreticalLine "Theoretical line"
                            :: acc
            in
            legendData flags newAcc LegendLimitRed

        LegendLimitRed ->
            let
                newAcc =
                    if flags.chart_type == "combined" then
                        acc

                    else
                        LegendElement LegendLimitRed "Static SD limits"
                            :: acc
            in
            legendData flags newAcc LegendLimitOrange

        LegendLimitOrange ->
            let
                newAcc =
                    if flags.chart_type /= "combined" then
                        acc

                    else
                        LegendElement LegendLimitOrange "Calculated SD limits"
                            :: acc
            in
            --last item so just reverse the list
            List.reverse newAcc


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


fixedFlagCheck : StatsData -> Bool
fixedFlagCheck s =
    -- check if it is a fixed chart
    case s.max of
        Nothing ->
            False

        Just v ->
            True


statStartTimes : Model -> List SectionData
statStartTimes model =
    List.map (\s -> SectionData (timify s.start_date) (fixedFlagCheck s))
        model.flags.stats


statStartTuples : Model -> List ( SectionData, Maybe SectionData )
statStartTuples model =
    tupleize (statStartTimes model)


flatten lst =
    List.concatMap identity lst



-- TODO: rename function


singleValidResults : Flags -> List RawCid
singleValidResults flags =
    flatten
        (List.map (\q -> q.values.valid) flags.classified_qcresults)


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
            flags.classified_qcresults
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
                singleDataList.values.valid
        )
        combinedData


readValidData : Flags -> List (List Datum)
readValidData flags =
    List.map
        (\classifiedSection ->
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
                classifiedSection.values.valid
        )
        flags.classified_qcresults


readInvalidData : Flags -> String -> List (List Datum)
readInvalidData flags selector =
    List.map
        (\classifiedSection ->
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
                (if selector == "above" then
                    classifiedSection.values.above_valid

                 else
                    classifiedSection.values.below_valid
                )
        )
        flags.classified_qcresults


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


scaleXY : Flags -> List (List Datum) -> Maybe BoundingBox2d -> ValuesClassification -> List (List ScaledPoint)
scaleXY flags combinedData boundingBox valuesClassification =
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
                    let
                        boo =
                            1

                        yValue =
                            case valuesClassification of
                                ValuesValid ->
                                    d.value

                                ValuesAbove ->
                                    flags.boundaries.above

                                ValuesBelow ->
                                    flags.boundaries.below
                    in
                    { point2d = Point2d.fromCoordinates ( doX cs d.time, doY cs yValue )
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


dataStats : Model -> DataStats
dataStats model =
    let
        values =
            List.map (\qr -> qr.c) (singleValidResults model.flags)

        mean =
            List.foldl (+) 0.0 values / toFloat (List.length values)

        sqrd =
            List.foldl (\v a -> a + ((v - mean) ^ 2)) 0.0 values

        sd =
            sqrt (sqrd / toFloat (List.length values - 1))
    in
    DataStats mean sd


deviations : Model -> Float -> (List Float -> Maybe Float) -> Float
deviations model x fn =
    let
        stats =
            model.flags.stats

        stats2 =
            if List.length stats == 0 then
                let
                    values =
                        List.map (\qr -> qr.c) (singleValidResults model.flags)

                    mean =
                        List.foldl (+) 0.0 values / toFloat (List.length values)
                in
                if List.length values == 0 then
                    [ { start_date = ""
                      , deviation = 25.0
                      , mean = 250.0
                      , nominal = 250.0
                      , min = Nothing
                      , max = Nothing
                      }
                    ]

                else
                    [ { start_date = ""
                      , deviation = standardDeviation model.flags
                      , mean = mean
                      , nominal = mean
                      , min = Nothing
                      , max = Nothing
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


tickBottom : Model -> Float
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
                singleValidResults flags
        in
        if List.length qcvalues == 0 then
            250.0

        else
            List.foldl (+) 0.0 (List.map (\qc -> qc.c) qcvalues)
                / toFloat (List.length qcvalues)

    else
        List.foldl (+) 0.0 meanValues / toFloat (List.length meanValues)


standardDeviation : Flags -> Float
standardDeviation flags =
    let
        values =
            List.map (\qc -> qc.c) (singleValidResults flags)

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
    -- fix that with the new boundaries
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
