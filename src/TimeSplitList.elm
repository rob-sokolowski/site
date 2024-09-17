module TimeSplitList exposing
    ( LLNode(..)
    , Next(..)
    , Node(..)
    , TimeSplitList
    , appendll
    , newTimeSplitList
    , tailOf
    , tailll
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
--                           /-----|| split split
--                  /-------x------|| split seq
--                 |   /-----------|| seq split
--       |---------x---x-----------|| seq seq
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


{-| tailOf a timelineIx, given some node

TODO: tail along a given TimelineIx?
TODO: tail along Set TimelineIx?
TODO: Result and Err instead of Maybe for return type
TODO: Maybe I should have tailOf vs tails???
TODO: elm-review rule for stack safety

-}
tailOf : Node a -> TimelineIx -> Maybe (Node a)
tailOf (Node_ node) tlix =
    case node.next of
        Just next ->
            case next of
                Seq next_ ->
                    tailOf next_ tlix

                Split ( Node_ lhs, Node_ rhs ) ->
                    if lhs.timelineIx == tlix then
                        tailOf (Node_ lhs) tlix

                    else if rhs.timelineIx == tlix then
                        tailOf (Node_ rhs) tlix

                    else
                        Nothing

        Nothing ->
            Just (Node_ node)


append : TimeSplitList a -> ( List (Node a), TimelineIx ) -> TimeSplitList a
append tree ( nodes, tix ) =
    case nodes of
        [] ->
            tree

        n :: ns ->
            append tree ( ns, tix + 1 )


appendAlongTimeline : Node a -> ( Node a, TimelineIx ) -> Node a
appendAlongTimeline list ( nodeToAppend, tix ) =
    case list of
        Node_ node_ ->
            case node_.next of
                Just nextNode ->
                    case nextNode of
                        Seq node__ ->
                            let
                                n =
                                    case node__ of
                                        Node_ n_ ->
                                            { n_ | next = Just (appendAlongTimeline node__ ( nodeToAppend, tix )) }
                            in
                            Node_ n

                        Split ( seq, split ) ->
                            { seq | next = Just (appendAlongTimeline split ( nodeToAppend, tix ) nodeToAppend) }

                -- Recursively call append on the next node
                Nothing ->
                    -- Found the end of the list, append new value here
                    Node_ { node_ | next = Just nodeToAppend }



--case node.next of
--    Just next ->
--        case next of
--            Seq node_ ->
--                appendAlongTimeline node_ ( nodeToAppend, tix )
--
--            Split ( lhs, rhs ) ->
--                appendAlongTimeline lhs ( nodeToAppend, tix )
-- begin region: simpler case


type LLNode a
    = LLNode_
        { val : a
        , next : Maybe (LLNode a)
        }


appendll : LLNode b -> LLNode b -> LLNode b
appendll list newVal =
    case list of
        LLNode_ node_ ->
            case node_.next of
                Just nextNode ->
                    -- Recursively call append on the next node
                    LLNode_ { node_ | next = Just (appendll nextNode newVal) }

                Nothing ->
                    -- Found the end of the list, append new value here
                    LLNode_ { node_ | next = Just newVal }


tailll : LLNode a -> LLNode a
tailll (LLNode_ head) =
    case head.next of
        Nothing ->
            LLNode_ head

        Just next_ ->
            tailll next_



-- end region: simpler case
