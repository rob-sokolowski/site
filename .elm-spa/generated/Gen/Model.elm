module Gen.Model exposing (Model(..))

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


type Model
    = Redirecting_
    | BouncingBall Gen.Params.BouncingBall.Params Pages.BouncingBall.Model
    | ElmUiSvgIssue Gen.Params.ElmUiSvgIssue.Params Pages.ElmUiSvgIssue.Model
    | Home_ Gen.Params.Home_.Params
    | ParableOfPolygonsClone Gen.Params.ParableOfPolygonsClone.Params Pages.ParableOfPolygonsClone.Model
    | Pops Gen.Params.Pops.Params Pages.Pops.Model
    | ScriptaDemo Gen.Params.ScriptaDemo.Params Pages.ScriptaDemo.Model
    | SpeedReadDemo Gen.Params.SpeedReadDemo.Params Pages.SpeedReadDemo.Model
    | VegaLite Gen.Params.VegaLite.Params Pages.VegaLite.Model
    | WordleClone Gen.Params.WordleClone.Params Pages.WordleClone.Model
    | Stories__ParableOfPolygonsQa Gen.Params.Stories.ParableOfPolygonsQa.Params Pages.Stories.ParableOfPolygonsQa.Model
    | NotFound Gen.Params.NotFound.Params

