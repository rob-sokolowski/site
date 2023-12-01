module TimeSplitList exposing
    ( Next(..)
    , Node(..)
    , TimeSplitList
    , newTimeSplitList
    )

-- This TimeSplitList is intended to support the time-splitting animations of the bouncing ball applet. As of writing,
-- I'm not entirely sure to name it, the working title is "TimeSplitList"
--
-- Features this needs to support:
-- 1) Clock runs without any disruption
-- 2) Clock runs without disruption, is re-winded to some frame prior, and animation is resumed
-- 3) Clock runs, intervention is proposed, two timelines proceed, one undisturbed, one disturbed exactly once (the intervention)
-- 4) Clock runs, intervention is proposed, two timelines proceed, one undisturbed, one disturbed. Another intervention is
--    proposed on the disturbed timeline resulting in a total of 3 timelines, labeled 0, 1, & 2:
--
--                           /-----|| 2
--                  /-------x------|| 1
--       |---------x---------------|| 0
--
-- 5) Repeat process of step 4, then rewind to some point between the first and second intervention. A 3rd intervention is
--    proposed at this instant, on the control timeline. The result is, note how there is still exactly 1 undisturbed
--    timeline, 0, and a new timeline, labeled 3:
--
--                           /-----|| 2
--                  /-------x------|| 1
--                 |   /-----------|| 3
--       |---------x---x-----------|| 0
--


type alias TreeListErr =
    String


type alias TimelineIx =
    Int


type alias FrameIx =
    Int


type alias TimeSplitList a =
    { head : Node a
    , timelineCount : Int
    }


type Node a
    = Node_
        { ix : FrameIx
        , timelineIx : TimelineIx
        , val : a
        , next : Maybe (Next a)
        }


type Next a
    = Seq (Node a)
    | Split ( Node a, Node a )


defaultNode =
    -- TODO: placeholder value until I solve degenerate cases, and wrap things in a Result
    Node_
        { ix = 0
        , timelineIx = 0
        , val = "One"
        , next = Nothing
        }


{-| Returns a newly initialized TimeSplitList, with exactly 1 node with val, and 1 timeline
-}
newTimeSplitList : a -> TimeSplitList a
newTimeSplitList val =
    { head =
        Node_
            { ix = 0
            , timelineIx = 0
            , val = val
            , next = Nothing
            }
    , timelineCount = 1
    }


{-| Starting at some node of the TimeSplitList, transverse along next until we reach _a_ head of the list, i.e., a node
that does not have a next node. Note: due to the splitting nature of the TimeSplitList, many such heads may exist.

TODO: tail along a given TimelineIx?
TODO: tail along Set TimelineIx?
TODO: Result and Err instead of Maybe for return type
TODO: Maybe I should have tailOf vs tails???

-}
tail : Node a -> TimelineIx -> Maybe (Node a)
tail (Node_ node) tlix =
    -- TODO: elm-review rule for stack safety
    case node.next of
        Just next ->
            case next of
                Seq next_ ->
                    tail next_ tlix

                Split ( Node_ lhs, Node_ rhs ) ->
                    if lhs.timelineIx == tlix then
                        tail (Node_ lhs) tlix

                    else if rhs.timelineIx == tlix then
                        tail (Node_ rhs) tlix

                    else
                        Nothing

        Nothing ->
            Nothing


append : TimeSplitList a -> List (Node a) -> Result TreeListErr (Node a)
append tree nodes =
    if tree.timelineCount == List.length nodes then
        Result.Err "How do I do this?!?!?"

    else
        Result.Err "When appending to a TimeSplitList, the number of nodes to be append must equal the number of timelines"


appendSub : TimeSplitList a -> TimelineIx -> Node a -> Result TreeListErr (Node a)
appendSub tree tx node =
    Result.Err "TODO!"
