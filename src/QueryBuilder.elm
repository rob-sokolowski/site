module QueryBuilder exposing (..)


type alias ColumnRef =
    String


type KimballColumn
    = KimballColumn_ ( ColumnRef, KimballClassification )


type KimballClassification
    = Dimension
    | Measure Aggregation
    | Time
    | Error


kimballClassificationToString : KimballClassification -> String
kimballClassificationToString kc =
    case kc of
        Dimension ->
            "dimension"

        Measure _ ->
            "measure"

        Time ->
            "time"

        Error ->
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
            List.map
                (\kc ->
                    case kc of
                        KimballColumn_ ( colRef, _ ) ->
                            colRef
                )
                (List.filter
                    (\e ->
                        case e of
                            KimballColumn_ ( _, kClass ) ->
                                case kClass of
                                    Dimension ->
                                        True

                                    _ ->
                                        False
                    )
                    kCols
                )

        groupByFields : List ColumnRef
        groupByFields =
            -- TODO: Tidy this (and above) up? Almost identical w/ Measure / Dim
            List.map
                (\kc ->
                    case kc of
                        KimballColumn_ ( colRef, _ ) ->
                            colRef
                )
                (List.filter
                    (\e ->
                        case e of
                            KimballColumn_ ( _, kClass ) ->
                                case kClass of
                                    Measure _ ->
                                        True

                                    _ ->
                                        False
                    )
                    kCols
                )
    in
    "select " ++ String.join "," selectFields ++ "," ++ String.join "," groupByFields ++ " from " ++ tRef
