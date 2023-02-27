module Gen.Model exposing (Model(..))

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


type Model
    = Redirecting_
    | Animation Gen.Params.Animation.Params Pages.Animation.Model
    | BouncingBall Gen.Params.BouncingBall.Params Pages.BouncingBall.Model
    | ElmUiSvgIssue Gen.Params.ElmUiSvgIssue.Params Pages.ElmUiSvgIssue.Model
    | GameDev Gen.Params.GameDev.Params Pages.GameDev.Model
    | Home_ Gen.Params.Home_.Params
    | ParableOfPolygonsClone Gen.Params.ParableOfPolygonsClone.Params Pages.ParableOfPolygonsClone.Model
    | Pops Gen.Params.Pops.Params Pages.Pops.Model
    | ScriptaDemo Gen.Params.ScriptaDemo.Params Pages.ScriptaDemo.Model
    | Snippets Gen.Params.Snippets.Params Pages.Snippets.Model
    | SpeedReadDemo Gen.Params.SpeedReadDemo.Params Pages.SpeedReadDemo.Model
    | VegaLite Gen.Params.VegaLite.Params Pages.VegaLite.Model
    | WordleClone Gen.Params.WordleClone.Params Pages.WordleClone.Model
    | Snippets__AntMarching Gen.Params.Snippets.AntMarching.Params Pages.Snippets.AntMarching.Model
    | Snippets__InactionIsAnAction Gen.Params.Snippets.InactionIsAnAction.Params Pages.Snippets.InactionIsAnAction.Model
    | Snippets__RequestResponseModes Gen.Params.Snippets.RequestResponseModes.Params Pages.Snippets.RequestResponseModes.Model
    | Stories__ParableOfPolygonsQa Gen.Params.Stories.ParableOfPolygonsQa.Params Pages.Stories.ParableOfPolygonsQa.Model
    | NotFound Gen.Params.NotFound.Params

