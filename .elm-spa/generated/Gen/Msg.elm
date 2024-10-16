module Gen.Msg exposing (Msg(..))

import Gen.Params.BouncingBall
import Gen.Params.ElmUiSvgIssue
import Gen.Params.Home_
import Gen.Params.IkedaPattern
import Gen.Params.ParableOfPolygonsClone
import Gen.Params.Pops
import Gen.Params.ScriptaDemo
import Gen.Params.SpeedReadDemo
import Gen.Params.WordleClone
import Gen.Params.Stories.AccelerometerTest
import Gen.Params.Stories.IframeTest
import Gen.Params.Stories.ParableOfPolygonsQa
import Gen.Params.NotFound
import Pages.BouncingBall
import Pages.ElmUiSvgIssue
import Pages.Home_
import Pages.IkedaPattern
import Pages.ParableOfPolygonsClone
import Pages.Pops
import Pages.ScriptaDemo
import Pages.SpeedReadDemo
import Pages.WordleClone
import Pages.Stories.AccelerometerTest
import Pages.Stories.IframeTest
import Pages.Stories.ParableOfPolygonsQa
import Pages.NotFound


type Msg
    = BouncingBall Pages.BouncingBall.Msg
    | ElmUiSvgIssue Pages.ElmUiSvgIssue.Msg
    | IkedaPattern Pages.IkedaPattern.Msg
    | ParableOfPolygonsClone Pages.ParableOfPolygonsClone.Msg
    | Pops Pages.Pops.Msg
    | ScriptaDemo Pages.ScriptaDemo.Msg
    | SpeedReadDemo Pages.SpeedReadDemo.Msg
    | WordleClone Pages.WordleClone.Msg
    | Stories__AccelerometerTest Pages.Stories.AccelerometerTest.Msg
    | Stories__IframeTest Pages.Stories.IframeTest.Msg
    | Stories__ParableOfPolygonsQa Pages.Stories.ParableOfPolygonsQa.Msg

