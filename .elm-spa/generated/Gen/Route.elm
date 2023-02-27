module Gen.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

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
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Animation
    | BouncingBall
    | ElmUiSvgIssue
    | GameDev
    | Home_
    | ParableOfPolygonsClone
    | Pops
    | ScriptaDemo
    | Snippets
    | SpeedReadDemo
    | VegaLite
    | WordleClone
    | Snippets__AntMarching
    | Snippets__InactionIsAnAction
    | Snippets__RequestResponseModes
    | Stories__ParableOfPolygonsQa
    | NotFound


fromUrl : Url -> Route
fromUrl =
    Parser.parse (Parser.oneOf routes) >> Maybe.withDefault NotFound


routes : List (Parser (Route -> a) a)
routes =
    [ Parser.map Home_ Gen.Params.Home_.parser
    , Parser.map Animation Gen.Params.Animation.parser
    , Parser.map BouncingBall Gen.Params.BouncingBall.parser
    , Parser.map ElmUiSvgIssue Gen.Params.ElmUiSvgIssue.parser
    , Parser.map GameDev Gen.Params.GameDev.parser
    , Parser.map ParableOfPolygonsClone Gen.Params.ParableOfPolygonsClone.parser
    , Parser.map Pops Gen.Params.Pops.parser
    , Parser.map ScriptaDemo Gen.Params.ScriptaDemo.parser
    , Parser.map Snippets Gen.Params.Snippets.parser
    , Parser.map SpeedReadDemo Gen.Params.SpeedReadDemo.parser
    , Parser.map VegaLite Gen.Params.VegaLite.parser
    , Parser.map WordleClone Gen.Params.WordleClone.parser
    , Parser.map NotFound Gen.Params.NotFound.parser
    , Parser.map Snippets__AntMarching Gen.Params.Snippets.AntMarching.parser
    , Parser.map Snippets__InactionIsAnAction Gen.Params.Snippets.InactionIsAnAction.parser
    , Parser.map Snippets__RequestResponseModes Gen.Params.Snippets.RequestResponseModes.parser
    , Parser.map Stories__ParableOfPolygonsQa Gen.Params.Stories.ParableOfPolygonsQa.parser
    ]


toHref : Route -> String
toHref route =
    let
        joinAsHref : List String -> String
        joinAsHref segments =
            "/" ++ String.join "/" segments
    in
    case route of
        Animation ->
            joinAsHref [ "animation" ]
    
        BouncingBall ->
            joinAsHref [ "bouncing-ball" ]
    
        ElmUiSvgIssue ->
            joinAsHref [ "elm-ui-svg-issue" ]
    
        GameDev ->
            joinAsHref [ "game-dev" ]
    
        Home_ ->
            joinAsHref []
    
        ParableOfPolygonsClone ->
            joinAsHref [ "parable-of-polygons-clone" ]
    
        Pops ->
            joinAsHref [ "pops" ]
    
        ScriptaDemo ->
            joinAsHref [ "scripta-demo" ]
    
        Snippets ->
            joinAsHref [ "snippets" ]
    
        SpeedReadDemo ->
            joinAsHref [ "speed-read-demo" ]
    
        VegaLite ->
            joinAsHref [ "vega-lite" ]
    
        WordleClone ->
            joinAsHref [ "wordle-clone" ]
    
        Snippets__AntMarching ->
            joinAsHref [ "snippets", "ant-marching" ]
    
        Snippets__InactionIsAnAction ->
            joinAsHref [ "snippets", "inaction-is-an-action" ]
    
        Snippets__RequestResponseModes ->
            joinAsHref [ "snippets", "request-response-modes" ]
    
        Stories__ParableOfPolygonsQa ->
            joinAsHref [ "stories", "parable-of-polygons-qa" ]
    
        NotFound ->
            joinAsHref [ "not-found" ]

