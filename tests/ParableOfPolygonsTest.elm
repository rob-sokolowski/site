module ParableOfPolygonsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Polygon of Parables test cases"
        [ describe "get neighboring cells"
            [ test "empty strings collapse to nothing"
                (\_ -> PromptParser.parseCellData "" |> Expect.equal Empty)
            ]
        ]


grid : Array2D Cell
