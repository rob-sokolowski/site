module QueryBuilder exposing (..)


type alias ColumnRef =
    String


type Granularity
    = Year
    | Quarter
    | Month
    | Week
    | Day
    | Hour
    | Minute


type TimeClass
    = Continuous
    | Discrete Granularity


type KimballColumn
    = Dimension ColumnRef
    | Measure Aggregation ColumnRef
    | Time TimeClass ColumnRef
    | Error ColumnRef


kimballClassificationToString : KimballColumn -> String
kimballClassificationToString kc =
    case kc of
        Dimension _ ->
            "dimension"

        Measure _ _ ->
            "measure"

        Time _ _ ->
            "time"

        Error _ ->
            "error"


type
    Aggregation
    -- TODO: Something to think about, do I want to support an `Unspecified` variant?
    --       That would add complexity, but may make the UX less "assume-y"
    = Sum
    | Mean
    | Median
    | Min
    | Max
    | Count
    | CountDistinct


type alias SqlStr =
    String


type alias TableRef =
    String


queryBuilder : List KimballColumn -> TableRef -> SqlStr
queryBuilder kCols tRef =
    let
        selectFields : List ColumnRef
        selectFields =
            List.filterMap
                (\e ->
                    case e of
                        Dimension colRef ->
                            Just colRef

                        Time timeClass colRef ->
                            case timeClass of
                                Continuous ->
                                    Just colRef

                                Discrete Year ->
                                    Just <| "date_trunc('year', " ++ colRef ++ ")"

                                Discrete Quarter ->
                                    Just <| "date_trunc('quarter', " ++ colRef ++ ")"

                                Discrete Month ->
                                    Just <| "date_trunc('month', " ++ colRef ++ ")"

                                Discrete Week ->
                                    Just <| "date_trunc('week', " ++ colRef ++ ")"

                                Discrete Day ->
                                    Just <| "date_trunc('day', " ++ colRef ++ ")"

                                Discrete Hour ->
                                    Just <| "date_trunc('hour', " ++ colRef ++ ")"

                                Discrete Minute ->
                                    Just <| "date_trunc('minute', " ++ colRef ++ ")"

                        _ ->
                            Nothing
                )
                kCols

        groupByFields : List ColumnRef
        groupByFields =
            List.filterMap
                (\e ->
                    case e of
                        Measure Sum colRef ->
                            Just <| "sum(" ++ colRef ++ ")"

                        Measure Mean colRef ->
                            Just <| "avg(" ++ colRef ++ ")"

                        Measure Median colRef ->
                            Just <| "median(" ++ colRef ++ ")"

                        Measure Count colRef ->
                            Just <| "count(" ++ colRef ++ ")"

                        Measure CountDistinct colRef ->
                            Just <| "count(distinct" ++ colRef ++ ")"

                        Measure Min colRef ->
                            Just <| "min(" ++ colRef ++ ")"

                        Measure Max colRef ->
                            Just <| "max(" ++ colRef ++ ")"

                        _ ->
                            Nothing
                )
                kCols
    in
    "select " ++ String.join "," selectFields ++ String.join "," groupByFields ++ " from " ++ tRef
