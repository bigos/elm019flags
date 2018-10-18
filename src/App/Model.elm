module App.Model exposing (AxisData, AxisX, AxisY, ChartRecord, ChartScalings, Datum, Flags, Model, Msg(..), RawCid, ScaledPoint, StatsData, Tooltip, TooltipData(..), averageMean, chartBottom, chartEnd, chartStart, chartTop, deviations, doX, doY, findStatForTime, hidev, init, largestDeviation, lodev, prepareTime, readData, scaleXY, setChartScalings, statStartTimes, statStartTuples, tempStats, tickBottom, toPoints, tupleize, tupleizeHelper)

import App.Utilities exposing (..)
import BoundingBox2d exposing (BoundingBox2d)
import ISO8601
import List.Extra
import Point2d exposing (Point2d)


type alias Model =
    { chartBoundingBox : Maybe BoundingBox2d
    , chartScalings : ChartScalings
    , chartType : String
    , data : List Datum
    , dateFrom : Maybe ISO8601.Time
    , dateTo : Maybe ISO8601.Time
    , flags : Flags
    , points : List Point2d
    , scaledPoints : List ScaledPoint
    , tooltip : Maybe Tooltip
    }


type alias Flags =
    { acqnominal : Float
    , analyteid : Int
    , chart_type : String
    , date_from : String
    , date_to : String
    , pdf : Bool
    , axes : AxisData
    , stats : List StatsData
    , maintenance_logs : List ChartRecord
    , reviews : List ChartRecord
    , qcresults : List RawCid
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
    }


type alias StatsData =
    { start_date : String
    , deviation : Float
    , mean : Float
    , nominal : Float
    }


type alias RawCid =
    { id : Int
    , c : Float
    , d : String
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


type alias Tooltip =
    { data : TooltipData
    , coordinates : ( Float, Float )
    , title : Maybe String
    }



-- UPDATE TYPES


type Msg
    = TooltipMouseEnter TooltipData ( Float, Float ) (Maybe String)
    | TooltipMouseLeave



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        data =
            readData flags

        points =
            toPoints data

        chartBoundingBox =
            BoundingBox2d.containingPoints points
    in
    ( { chartBoundingBox = chartBoundingBox
      , chartScalings = setChartScalings flags chartBoundingBox
      , chartType = "default"
      , data = data
      , dateFrom = prepareTime flags.date_from
      , dateTo = prepareTime flags.date_to
      , flags = flags
      , points = points
      , scaledPoints = scaleXY flags data chartBoundingBox
      , tooltip = Nothing
      }
    , Cmd.none
    )


tempStats : List StatsData
tempStats =
    [ { deviation = 37.5, mean = 250, nominal = 250, start_date = "2011-07-29T00:00:00Z" }, { deviation = 15, mean = 245.2, nominal = 250, start_date = "2014-10-17T00:00:00Z" } ]


findStatForTime : List StatsData -> Int -> Maybe StatsData
findStatForTime stats time =
    let
        possibleStats =
            List.Extra.takeWhile (\s -> timify s.start_date <= time) stats

        res =
            List.Extra.last possibleStats
    in
    Debug.log (">>stats results for time>> " ++ Debug.toString res ++ " << " ++ untimify time ++ " <<<<<|\n")
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
    tupleize (statStartTimes model)


readData : Flags -> List Datum
readData flags =
    List.map (\d -> Datum (toFloat (timify d.d)) d.c) flags.qcresults


toPoints : List Datum -> List Point2d
toPoints data =
    List.map (\d -> Point2d.fromCoordinates ( d.time, d.value )) data


prepareTime : String -> Maybe ISO8601.Time
prepareTime s =
    case ISO8601.fromString s of
        Err msg ->
            Nothing

        Result.Ok d ->
            Just d


scaleXY : Flags -> List Datum -> Maybe BoundingBox2d -> List ScaledPoint
scaleXY flags data boundingBox =
    let
        cs =
            setChartScalings flags boundingBox

        points =
            toPoints data
    in
    List.map
        (\d ->
            { point2d = Point2d.fromCoordinates ( doX cs d.time, doY cs d.value )
            , datum = d
            }
        )
        data


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



-- change me taking into consideration average mean and largest deviation


chartBottom : Model -> Float
chartBottom model =
    doY model.chartScalings (deviations model lodev List.minimum)


deviations model x fn =
    let
        devs =
            List.map (\s -> s.mean + s.deviation * x) model.flags.stats

        calc =
            fn devs
    in
    Maybe.withDefault 0.0 calc


hidev =
    6.0


lodev =
    -4.5


chartTop : Model -> Float
chartTop model =
    doY model.chartScalings (deviations model 5 List.maximum)



-- change me taking into consideration average mean and largest deviation


tickBottom model =
    doY model.chartScalings (deviations model -4.7 List.minimum)


averageMean : Flags -> Float
averageMean flags =
    let
        meanValues =
            List.map (\s -> s.mean) flags.stats
    in
    List.foldl (+) 0.0 meanValues / toFloat (List.length meanValues)


largestDeviation : Flags -> Float
largestDeviation flags =
    let
        deviationValues =
            List.map (\s -> s.deviation) flags.stats
    in
    Maybe.withDefault 0.0 (List.maximum deviationValues)


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
