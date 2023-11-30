module TimeSplitListTest exposing (..)

import Expect
import Test exposing (..)
import TimeSplitList exposing (Next(..), Node(..), TimeSplitList, newTreeList)


suite : Test
suite =
    describe "Tree List data structure"
        [ describe "Creating, appending, splitting, etc."
            [ test "Initialize new TimeSplitList"
                (\_ ->
                    newTreeList "One"
                        |> Expect.equal
                            initTimeSplitListOfString
                )
            ]
        ]


initTimeSplitListOfString : TimeSplitList String
initTimeSplitListOfString =
    { head =
        Node_
            { ix = 0
            , timelineIx = 0
            , val = "One"
            , next = Nothing

            --, prev = Nothing
            }
    , timelineCount = 1
    }


node_0_0 : Node String
node_0_0 =
    Node_
        { timelineIx = 0
        , ix = 0
        , val = "0-0"
        , next = Just (Seq node_0_1)

        --, prev = Nothing
        }


node_0_1 : Node String
node_0_1 =
    Node_
        { timelineIx = 0
        , ix = 1
        , val = "0-1"
        , next = Just (Split ( node_0_2, node_1_2 ))

        --, prev = Just node_0_0
        }


node_0_2 : Node String
node_0_2 =
    Node_
        { timelineIx = 0
        , ix = 2
        , val = "0-2"
        , next = Just (Seq node_0_3)

        --, prev = Just node_0_1
        }


node_1_2 : Node String
node_1_2 =
    Node_
        { timelineIx = 1
        , ix = 2
        , val = "1-2"
        , next = Just (Seq node_1_3)

        --, prev = Just node_0_1
        }


node_0_3 : Node String
node_0_3 =
    Node_
        { timelineIx = 0
        , ix = 3
        , val = "0-3"
        , next = Nothing

        --, prev = Just node_0_2
        }


node_1_3 : Node String
node_1_3 =
    Node_
        { timelineIx = 1
        , ix = 3
        , val = "1-3"
        , next = Nothing

        --, prev = Just node_1_2
        }


simpleTwoTimelineTreeList : TimeSplitList String
simpleTwoTimelineTreeList =
    { head = node_0_0
    , timelineCount = 2
    }
