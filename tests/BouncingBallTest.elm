module BouncingBallTest exposing (..)

import Expect
import Pages.BouncingBall exposing (Model, RunningState(..), computeNextPos)
import Test exposing (..)


dt =
    30


suite : Test
suite =
    describe "Bouncing ball app"
        [ describe "Play/pause resume logic"
            [ test "simulation is paused, no changes should happen"
                (\_ ->
                    computeNextPos case1 dt
                        |> Expect.equal ( case1.ballPos, case1.hist, case1.currentFrame )
                )
            , test "simulation is playing, no historic recall"
                (\_ ->
                    computeNextPos case2 dt
                        |> Expect.equal ( { rx = 1, ry = 1.725, vx = 1, vy = -1.4292, x = 10.03, y = 9.957124 }, [ { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 }, { rx = 1, ry = 1.725, vx = 1, vy = -1.4292, x = 10.03, y = 9.957124 } ], 2 )
                )
            , test "simulation is playing, but via historic recall"
                (\_ ->
                    computeNextPos case3 dt
                        |> Expect.equal ( { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 }, [ { rx = 1, ry = 1, vx = 1, vy = 1, x = 9, y = 11 } ], 1 )
                )
            ]
        ]


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
