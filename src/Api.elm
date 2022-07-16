module Api exposing (..)

import Utils exposing (removeNothingFromList)


type alias Column =
    { name : String
    , type_ : String
    , vals : List (Maybe Val)
    }


type Val
    = Varchar_ String
    | Bool__ Bool
    | Float__ Float
    | Int__ Int
    | Unknown


type alias TableRef =
    String


type alias DuckDbQueryResponse =
    { columns : List Column
    }


type alias DuckDbTableRefsResponse =
    { refs : List TableRef
    }


type alias Column2 val =
    { name : String
    , vals : List val
    }


mapColToStringCol : Column -> Column2 String
mapColToStringCol col =
    case col.type_ of
        "VARCHAR" ->
            let
                vals_ =
                    removeNothingFromList col.vals

                mapToStringList : List String -> List Val -> List String
                mapToStringList accum vals__ =
                    case vals__ of
                        [ Varchar_ v ] ->
                            accum ++ [ v ]

                        (Varchar_ v) :: vs ->
                            [ v ] ++ mapToStringList accum vs

                        _ ->
                            []
            in
            { name = col.name
            , vals = mapToStringList [] vals_
            }

        _ ->
            { name = col.name
            , vals = []
            }


mapColToFloatCol : Column -> Column2 Float
mapColToFloatCol col =
    case col.type_ of
        "DOUBLE" ->
            let
                vals_ =
                    removeNothingFromList col.vals

                mapToFloatList : List Float -> List Val -> List Float
                mapToFloatList accum vals__ =
                    case vals__ of
                        [ Float__ f ] ->
                            accum ++ [ f ]

                        (Float__ f) :: [ Float__ fs ] ->
                            [ f ] ++ mapToFloatList accum [ Float__ fs ]

                        _ ->
                            []
            in
            { name = col.name
            , vals = mapToFloatList [] vals_
            }

        _ ->
            { name = col.name
            , vals = []
            }
