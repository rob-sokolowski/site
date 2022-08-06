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
                    arr2d
                        |> getValueAt ( 1, 2 )
                        |> Expect.equal (Just 109)
                )
            , test "build an Array2D from list of lists, set a value at (rix, cix)"
                (\_ ->
                    arr2d
                        |> setValueAt ( 1, 2 ) -100
                        |> getValueAt ( 1, 2 )
                        |> Expect.equal (Just -100)
                )
            , test "build an Array2D from list of lists, get row at rix"
                (\_ ->
                    arr2d
                        |> getRow 1
                        |> Expect.equal (A.fromList [ 107, 108, 109 ])
                )
            , test "build an Array2D from list of lists, get col at cix"
                (\_ ->
                    arr2d
                        |> getCol 1
                        |> Expect.equal (A.fromList [ 91, 108 ])
                )
            , test "Array2D to a listOfLists, by transforming to Array2D and back to itself"
                (\_ ->
                    arr2d
                        |> toListOfLists
                        |> Expect.equal listOfList
                )
            , test "Is this element a member of our Array2d? False case"
                (\_ ->
                    arr2d
                        -- just a number unlikely to accidentally wind up in our Array2D
                        |> member -172648256
                        |> Expect.equal False
                )
            , test "Is this element a member of our Array2d? True cases"
                (\_ ->
                    Expect.all (List.map (\i -> Expect.equal (member i arr2d)) flattenedList) True
                )
            , test "Flatten Array2D to an Array"
                (\_ ->
                    flatten arr2d
                        |> Expect.equal (A.fromList [ 90, 91, 92, 107, 108, 109 ])
                )
            ]
        ]



-- begin region test data
--
-- NB: listOfList should be the only stated data in this region, the other functions are transformations of this data
--     for testing convenience


listOfList : List (List number)
listOfList =
    [ [ 90, 91, 92 ]
    , [ 107, 108, 109 ]
    ]


arr2d : Array2D number
arr2d =
    -- return our test dataset as an Array2D
    fromListOfLists listOfList


flattenedList : List number
flattenedList =
    -- return our test dataset as a flattened list, this can be used to parameterize tests
    A.toList <| flatten arr2d



-- end region test data
