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


type Aggregation
    = Unspecified
    | Sum
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

                        _ ->
                            Nothing
                )
                kCols

        groupByFields : List ColumnRef
        groupByFields =
            List.filterMap
                (\e ->
                    case e of
                        Measure _ colRef ->
                            Just colRef

                        _ ->
                            Nothing
                )
                kCols
    in
    "select " ++ String.join "," selectFields ++ "," ++ String.join "," groupByFields ++ " from " ++ tRef
