module BouncingBallTest exposing (..)

import Expect
import Pages.BouncingBall exposing (Model, RunningState(..), computeNextPos)
import Test exposing (..)


suite : Test
suite =
    describe "Bouncing ball app"
        [ describe "Play/pause resume logic"
            [ test "simulation is paused, no changes should happen"
                (\_ ->
                    computeNextPos case1
                        |> Expect.equal ( case1.ballPos, case1.hist, case1.currentFrame )
                )
            , test "simulation is playing, no historic recalld"
                (\_ ->
                    computeNextPos case2
                        |> Expect.equal ( { rx = 1, ry = 1.725, vx = 1, vy = -1.488, x = 10.033333333333333, y = 9.9504 }, [ { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 }, { rx = 1, ry = 1.725, vx = 1, vy = -1.488, x = 10.033333333333333, y = 9.9504 } ], 2 )
                )
            , test "simulation is playing, but via historic recall"
                (\_ ->
                    computeNextPos case3
                        |> Expect.equal ( { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 }, [ { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 } ], 1 )
                )
            ]
        ]



-- cases:
-- state is paused
--
-- state is playing, and is at new frames are to be simulated
--
-- state is playing, and next frame is to be recalled from history
--


case1 : Model
case1 =
    { ballPos =
        { x = 10
        , y = 10
        , rx = 1
        , ry = 1
        , vx = 1
        , vy = 1
        }
    , g = -1
    , runningState = Paused
    , currentFrame = 1
    , hist =
        [ { x = 9
          , y = 11
          , rx = 1
          , ry = 1
          , vx = 1
          , vy = 1
          }
        ]
    }


case2 : Model
case2 =
    { case1 | runningState = Playing }


case3 : Model
case3 =
    { case1 | runningState = Playing, currentFrame = 0 }
