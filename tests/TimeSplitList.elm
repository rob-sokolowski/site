module TimeSplitList exposing (..)

import Expect
import Test exposing (..)
import TimeSplitList exposing (Node(..), newTreeList)


suite : Test
suite =
    describe "Tree List data structure"
        [ describe "Creating, appending, splitting, etc."
            [ test "Initialize new TimeSplitList"
                (\_ ->
                    newTreeList "One"
                        |> Expect.equal
                            { head =
                                Node_
                                    { ix = 0
                                    , val = "One"
                                    , next = Nothing
                                    , prev = Nothing
                                    }
                            , timelineCount = 1
                            }
                )
            ]
        ]
