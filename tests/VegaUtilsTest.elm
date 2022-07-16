module VegaUtilsTest exposing (..)

import Api exposing (Column, Val(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import VegaUtils exposing (ColumnParamed, mapColToFloatCol, mapColToIntegerCol, mapColToStringCol)


suite : Test
suite =
    describe "VegaUtils module"
        [ describe "mapping types"
            [ test "string"
                (\_ ->
                    mapColToStringCol stringColumn
                        |> Expect.equal colParamedString
                )
            , test "int"
                (\_ ->
                    mapColToIntegerCol integerColumn
                        |> Expect.equal colParamedInt
                )
            , test "FLOAT"
                (\_ ->
                    mapColToFloatCol floatColumn
                        |> Expect.equal colParamedFloat
                )
            ]
        ]


stringColumn =
    { name = "a string column"
    , type_ = "VARCHAR"
    , vals =
        [ Just (Varchar_ "one")
        , Just (Varchar_ "two")
        , Just (Varchar_ "three")
        ]
    }


colParamedString : ColumnParamed String
colParamedString =
    { name = "a string column"
    , vals = [ "one", "two", "three" ]
    }


integerColumn =
    { name = "an int column"
    , type_ = "INTEGER"
    , vals =
        [ Just (Int_ 1)
        , Just (Int_ 2)
        , Just (Int_ 3)
        ]
    }


colParamedInt : ColumnParamed Int
colParamedInt =
    { name = "an int column"
    , vals = [ 1, 2, 3 ]
    }


floatColumn =
    { name = "a float column"
    , type_ = "DOUBLE"
    , vals =
        [ Just (Float_ 3.1)
        , Just (Float_ 3.14)
        , Just (Float_ 3.2)
        ]
    }


colParamedFloat : ColumnParamed Float
colParamedFloat =
    { name = "a float column"
    , vals = [ 3.1, 3.14, 3.2 ]
    }
