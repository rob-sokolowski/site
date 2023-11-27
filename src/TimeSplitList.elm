module TimeSplitList exposing (Next, Node(..), newTreeList)

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
        , val : a
        , next : Maybe (Next a)
        , prev : Maybe (Node a)
        }


type Next a
    = Seq (Node a)
    | Split ( Node a, Node a )


{-| Returns a newly initialized TimeSplitList, with exactly 1 node with val, and 1 timeline
-}
newTreeList : a -> TimeSplitList a
newTreeList val =
    { head =
        Node_
            { ix = 0
            , val = val
            , next = Nothing
            , prev = Nothing
            }
    , timelineCount = 1
    }


{-| Starting at some node of the TimeSplitList, transverse along prev until we reach the head of the list, i.e., the node
that does not have a previous node. There should be exactly one such node per list.
-}
head : Node a -> Node a
head (Node_ node) =
    case node.prev of
        -- TODO: elm-review rule for stack safety
        Just node_ ->
            head node_

        Nothing ->
            Node_ node


{-| Starting at some node of the TimeSplitList, transverse along next until we reach _a_ head of the list, i.e., a node
that does not have a next node. Note: due to the splitting nature of the TimeSplitList, many such heads may exist.

TODO: tail along a given TimelineIx?
TODO: tail along Set TimelineIx?

-}
tail : Node a -> Node a
tail (Node_ node) =
    -- TODO: elm-review rule for stack safety
    case node.next of
        Just next ->
            case next of
                Seq next_ ->
                    tail next_

                Split ( lhs, rhs ) ->
                    tail lhs

        Nothing ->
            Node_ node


append : TimeSplitList a -> List (Node a) -> Result TreeListErr (Node a)
append tree nodes =
    if tree.timelineCount == List.length nodes then
        Result.Err "How do I do this?!?!?"

    else
        Result.Err "When appending to a TimeSplitList, the number of nodes to be append must equal the number of timelines"


appendSub : TimeSplitList a -> TimelineIx -> Node a -> Result TreeListErr (Node a)
appendSub tree tx node =
    Result.Err "TODO!"



--foldl : (TimeSplitList a -> (Node a, TimelineIx) -> TimeSplitList a) -> TimeSplitList a -> TimeSplitList a -> b
--foldl func acc list =
--  case list of
--    [] ->
--      acc
--
--    x :: xs ->
--      foldl func (func x acc) xs
