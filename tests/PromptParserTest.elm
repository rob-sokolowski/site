module PromptParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import PromptParser
import SheetModel exposing (CellElement(..))
import Test exposing (..)


suite : Test
suite =
    describe "Parsing module"
        [ describe "behavior of parsing cell data"
            [ test "empty strings collapse to nothing"
                (\_ -> PromptParser.parseCellData "" |> Expect.equal Empty)
            , test "repeating whitespace strings collapse to nothing"
                (\_ -> PromptParser.parseCellData "    " |> Expect.equal Empty)
            , test "symbols are Empty.. for now"
                (\_ -> PromptParser.parseCellData "#" |> Expect.equal Empty)
            , test "symbols are Empty.. for now - padding with spaces"
                (\_ -> PromptParser.parseCellData "  /    " |> Expect.equal Empty)
            , test "symbols are Empty.. for now - with leading with spaces"
                (\_ -> PromptParser.parseCellData "     /" |> Expect.equal Empty)
            , test "symbols are Empty.. for now - with trailing spaces"
                (\_ -> PromptParser.parseCellData "/ " |> Expect.equal Empty)
            , test "simple int"
                (\_ -> PromptParser.parseCellData "7" |> Expect.equal (Int_ 7))
            , test "int with leading spaces"
                (\_ -> PromptParser.parseCellData "  7" |> Expect.equal (Int_ 7))
            , test "int with trailing spaces"
                (\_ -> PromptParser.parseCellData "7  " |> Expect.equal (Int_ 7))
            , test "int with leading & trailing spaces"
                (\_ -> PromptParser.parseCellData "  7  " |> Expect.equal (Int_ 7))

            -- TDOO: Need to work on the parser more..these are failing tests
            --, test "simple float"
            --    (\_ -> PromptParser.parseCellData "3.14" |> Expect.equal (Float_ 3.14))
            --, test "float with leading spaces"
            --    (\_ -> PromptParser.parseCellData "  3.14" |> Expect.equal (Float_ 3.14))
            --, test "float with trailing spaces"
            --    (\_ -> PromptParser.parseCellData "3.14  " |> Expect.equal (Float_ 3.14))
            --, test "float with leading & trailing spaces"
            --    (\_ -> PromptParser.parseCellData "  3.14  " |> Expect.equal (Float_ 3.14))
            ]
        ]
