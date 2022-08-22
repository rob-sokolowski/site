module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Utils exposing (collapseWhitespace)


suite : Test
suite =
    describe "Utils"
        [ describe "reduce whitespace - untrimmed cases"
            [ test "case 1"
                (\_ -> collapseWhitespace "\t\nsome      \t space" False |> Expect.equal " some space")
            , test "case 2"
                (\_ -> collapseWhitespace "\t\nsome      \t space\n" False |> Expect.equal " some space ")
            , test "case 3"
                (\_ -> collapseWhitespace "\t\nsome  \n    \t space" False |> Expect.equal " some space")
            ]
        , describe "reduce whitespace - trimmed cases"
            [ test "case 1"
                (\_ -> collapseWhitespace "\t\nsome      \t space" True |> Expect.equal "some space")
            , test "case 2"
                (\_ -> collapseWhitespace "\t\nsome      \t space\n" True |> Expect.equal "some space")
            , test "case 3"
                (\_ -> collapseWhitespace "\t\nsome  \n    \t space" True |> Expect.equal "some space")
            ]
        ]
