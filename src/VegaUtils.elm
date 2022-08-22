module VegaUtils exposing (..)

import Api exposing (Column, Val(..))
import Utils exposing (removeNothingsFromList)


type alias ColumnParamed val =
    { ref : String
    , vals : List val
    }


mapColToStringCol : Column -> ColumnParamed String
mapColToStringCol col =
    case col.type_ of
        "VARCHAR" ->
            let
                vals_ =
                    removeNothingsFromList col.vals

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
            { ref = col.ref
            , vals = mapToStringList [] vals_
            }

        _ ->
            { ref = "ERROR - STRING MAP"
            , vals = []
            }


mapColToFloatCol : Column -> ColumnParamed Float
mapColToFloatCol col =
    case col.type_ of
        "DOUBLE" ->
            let
                vals_ =
                    removeNothingsFromList col.vals

                mapToFloatList : List Float -> List Val -> List Float
                mapToFloatList accum vals__ =
                    case vals__ of
                        [ Float_ f ] ->
                            accum ++ [ f ]

                        (Float_ f) :: fs ->
                            [ f ] ++ mapToFloatList accum fs

                        _ ->
                            []
            in
            { ref = col.ref
            , vals = mapToFloatList [] vals_
            }

        _ ->
            { ref = "ERROR - FLOAT MAP"
            , vals = []
            }


mapColToIntegerCol : Column -> ColumnParamed Int
mapColToIntegerCol col =
    case col.type_ of
        "INTEGER" ->
            let
                vals_ =
                    removeNothingsFromList col.vals

                mapToIntList : List Int -> List Val -> List Int
                mapToIntList accum vals__ =
                    case vals__ of
                        [ Int_ i ] ->
                            accum ++ [ i ]

                        (Int_ i) :: is ->
                            [ i ] ++ mapToIntList accum is

                        _ ->
                            []
            in
            { ref = col.ref
            , vals = mapToIntList [] vals_
            }

        _ ->
            { ref = "ERROR - INT MAP"
            , vals = []
            }
