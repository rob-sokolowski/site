module Gen.Msg exposing (Msg(..))

import Gen.Params.Animation
import Gen.Params.BouncingBall
import Gen.Params.ElmUiSvgIssue
import Gen.Params.GameDev
import Gen.Params.Home_
import Gen.Params.ParableOfPolygonsClone
import Gen.Params.Pops
import Gen.Params.ScriptaDemo
import Gen.Params.Snippets
import Gen.Params.SpeedReadDemo
import Gen.Params.VegaLite
import Gen.Params.WordleClone
import Gen.Params.Snippets.AntMarching
import Gen.Params.Snippets.InactionIsAnAction
import Gen.Params.Snippets.RequestResponseModes
import Gen.Params.Stories.ParableOfPolygonsQa
import Gen.Params.NotFound
import Pages.Animation
import Pages.BouncingBall
import Pages.ElmUiSvgIssue
import Pages.GameDev
import Pages.Home_
import Pages.ParableOfPolygonsClone
import Pages.Pops
import Pages.ScriptaDemo
import Pages.Snippets
import Pages.SpeedReadDemo
import Pages.VegaLite
import Pages.WordleClone
import Pages.Snippets.AntMarching
import Pages.Snippets.InactionIsAnAction
import Pages.Snippets.RequestResponseModes
import Pages.Stories.ParableOfPolygonsQa
import Pages.NotFound


type Msg
    = Animation Pages.Animation.Msg
    | BouncingBall Pages.BouncingBall.Msg
    | ElmUiSvgIssue Pages.ElmUiSvgIssue.Msg
    | GameDev Pages.GameDev.Msg
    | ParableOfPolygonsClone Pages.ParableOfPolygonsClone.Msg
    | Pops Pages.Pops.Msg
    | ScriptaDemo Pages.ScriptaDemo.Msg
    | Snippets Pages.Snippets.Msg
    | SpeedReadDemo Pages.SpeedReadDemo.Msg
    | VegaLite Pages.VegaLite.Msg
    | WordleClone Pages.WordleClone.Msg
    | Snippets__AntMarching Pages.Snippets.AntMarching.Msg
    | Snippets__InactionIsAnAction Pages.Snippets.InactionIsAnAction.Msg
    | Snippets__RequestResponseModes Pages.Snippets.RequestResponseModes.Msg
    | Stories__ParableOfPolygonsQa Pages.Stories.ParableOfPolygonsQa.Msg

