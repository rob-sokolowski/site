module Gen.Msg exposing (Msg(..))

import Gen.Params.BouncingBall
import Gen.Params.ElmUiSvgIssue
import Gen.Params.Home_
import Gen.Params.ParableOfPolygonsClone
import Gen.Params.Pops
import Gen.Params.ScriptaDemo
import Gen.Params.SpeedReadDemo
import Gen.Params.VegaLite
import Gen.Params.WordleClone
import Gen.Params.Stories.ParableOfPolygonsQa
import Gen.Params.NotFound
import Pages.BouncingBall
import Pages.ElmUiSvgIssue
import Pages.Home_
import Pages.ParableOfPolygonsClone
import Pages.Pops
import Pages.ScriptaDemo
import Pages.SpeedReadDemo
import Pages.VegaLite
import Pages.WordleClone
import Pages.Stories.ParableOfPolygonsQa
import Pages.NotFound


type Msg
    = BouncingBall Pages.BouncingBall.Msg
    | ElmUiSvgIssue Pages.ElmUiSvgIssue.Msg
    | ParableOfPolygonsClone Pages.ParableOfPolygonsClone.Msg
    | Pops Pages.Pops.Msg
    | ScriptaDemo Pages.ScriptaDemo.Msg
    | SpeedReadDemo Pages.SpeedReadDemo.Msg
    | VegaLite Pages.VegaLite.Msg
    | WordleClone Pages.WordleClone.Msg
    | Stories__ParableOfPolygonsQa Pages.Stories.ParableOfPolygonsQa.Msg

