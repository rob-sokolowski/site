module TimeSplitListTest exposing (..)

import Expect
import Test exposing (..)
import TimeSplitList exposing (LLNode(..), Next(..), Node(..), TimeSplitList, appendll, newTimeSplitList, tailOf, tailll)


suite : Test
suite =
    describe "Tree List data structure"
        [ describe "Creating, appending, splitting, etc."
            [ test "Initialize new TimeSplitList"
                (\_ ->
                    newTimeSplitList "One"
                        |> Expect.equal
                            initTimeSplitListOfString
                )
            , test "tailOf simpleTwoTimelineTreeList - timelineIx=0"
                (\_ ->
                    tailOf simpleTwoTimelineTreeList.head 0
                        |> Expect.equal
                            (Just <|
                                Node_
                                    { timelineIx = 0
                                    , ix = 3
                                    , val = "0-3"
                                    , next = Nothing
                                    }
                            )
                )
            , test "tailOf simpleTwoTimelineTreeList - timelineIx=1"
                (\_ ->
                    tailOf simpleTwoTimelineTreeList.head 1
                        |> Expect.equal
                            (Just <|
                                Node_
                                    { timelineIx = 1
                                    , ix = 3
                                    , val = "1-3"
                                    , next = Nothing
                                    }
                            )
                )
            , test "tailOf simpleTwoTimelineTreeList - timelineIx=100"
                -- TODO: This is returning Nothing, but is it doing so for the right reasons?
                (\_ ->
                    tailOf simpleTwoTimelineTreeList.head 100
                        |> Expect.equal Nothing
                )
            ]
        , describe "Simpler Linked List"
            [ test "Seek tail of a linked list"
                (\_ ->
                    tailll simpleLinkedList
                        |> Expect.equal
                            (LLNode_
                                { val = "Jing", next = Nothing }
                            )
                )
            , test "Append tail of a linked list"
                (\_ ->
                    tailll (appendll simpleLinkedList russNode)
                        |> Expect.equal
                            (LLNode_
                                { val = "Russ", next = Nothing }
                            )
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
            }
    , timelineCount = 1
    }


{-| Returns the following TimeSplitList for some test cases:

     /-- 1
    -x-- 0

That is, there are 4 frames, with ixes; 0, 1, 2, 3
And two timelines with ixes; 0, 1

Timeline 0 splits on frameIx 1, with its first independent frame
starting on timeIx 2, continuing onto timeIx 3

-}
simpleTwoTimelineTreeList : TimeSplitList String
simpleTwoTimelineTreeList =
    let
        node_0_0 : Node String
        node_0_0 =
            Node_
                { timelineIx = 0
                , ix = 0
                , val = "0-0"
                , next = Just (Seq node_0_1)
                }

        node_0_1 : Node String
        node_0_1 =
            Node_
                { timelineIx = 0
                , ix = 1
                , val = "0-1"
                , next = Just (Split ( node_0_2, node_1_2 ))
                }

        node_0_2 : Node String
        node_0_2 =
            Node_
                { timelineIx = 0
                , ix = 2
                , val = "0-2"
                , next = Just (Seq node_0_3)
                }

        node_1_2 : Node String
        node_1_2 =
            Node_
                { timelineIx = 1
                , ix = 2
                , val = "1-2"
                , next = Just (Seq node_1_3)
                }

        node_0_3 : Node String
        node_0_3 =
            Node_
                { timelineIx = 0
                , ix = 3
                , val = "0-3"
                , next = Nothing
                }

        node_1_3 : Node String
        node_1_3 =
            Node_
                { timelineIx = 1
                , ix = 3
                , val = "1-3"
                , next = Nothing
                }
    in
    { head = node_0_0
    , timelineCount = 2
    }


russNode : LLNode String
russNode =
    LLNode_ { val = "Russ", next = Nothing }


simpleLinkedList : LLNode String
simpleLinkedList =
    LLNode_
        { val = "Rob"
        , next =
            Just
                (LLNode_
                    { val = "Jing"
                    , next = Nothing
                    }
                )
        }
