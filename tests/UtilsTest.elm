module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Utils exposing (collapseWhitespace)


suite : Test
suite =
    describe "Utils"
        [ describe "reduce whitespace in a string to single space"
            [ test "case 1"
                (\_ -> collapseWhitespace "\t\nsome      \t space" |> Expect.equal " some space")
            , test "case 2"
                (\_ -> collapseWhitespace "\t\nsome      \t space\n" |> Expect.equal " some space ")
            , test "case 3"
                (\_ -> collapseWhitespace "\t\nsome  \n    \t space" |> Expect.equal " some space")
            ]
        ]
