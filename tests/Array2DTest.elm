module Array2DTest exposing (..)

import Array as A
import Array2D exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Array2D data module"
        [ describe "Array2D data structure"
            [ test "build an Array2D from list of lists, get a value at (rix, cix)"
                (\_ ->
                    listOfList
                        |> fromListOfLists
                        |> getValueAt ( 1, 2 )
                        |> Expect.equal (Just 109)
                )
            , test "build an Array2D from list of lists, set a value at (rix, cix)"
                (\_ ->
                    listOfList
                        |> fromListOfLists
                        |> setValueAt ( 1, 2 ) -100
                        |> getValueAt ( 1, 2 )
                        |> Expect.equal (Just -100)
                )
            , test "build an Array2D from list of lists, get row at rix"
                (\_ ->
                    listOfList
                        |> fromListOfLists
                        |> getRow 1
                        |> Expect.equal (A.fromList [ 107, 108, 109 ])
                )
            , test "build an Array2D from list of lists, get col at cix"
                (\_ ->
                    listOfList
                        |> fromListOfLists
                        |> getCol 1
                        |> Expect.equal (A.fromList [ 91, 108 ])
                )
            , test "Array2D to a listOfLists, by transforming to Array2D and back to itself"
                (\_ ->
                    listOfList
                        |> fromListOfLists
                        |> toListOfLists
                        |> Expect.equal listOfList
                )
            ]
        ]


listOfList : List (List number)
listOfList =
    [ [ 90, 91, 92 ]
    , [ 107, 108, 109 ]
    ]
