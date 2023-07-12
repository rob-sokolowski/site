module Gen.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

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
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = BouncingBall
    | ElmUiSvgIssue
    | Home_
    | ParableOfPolygonsClone
    | Pops
    | ScriptaDemo
    | SpeedReadDemo
    | VegaLite
    | WordleClone
    | Stories__ParableOfPolygonsQa
    | NotFound


fromUrl : Url -> Route
fromUrl =
    Parser.parse (Parser.oneOf routes) >> Maybe.withDefault NotFound


routes : List (Parser (Route -> a) a)
routes =
    [ Parser.map Home_ Gen.Params.Home_.parser
    , Parser.map BouncingBall Gen.Params.BouncingBall.parser
    , Parser.map ElmUiSvgIssue Gen.Params.ElmUiSvgIssue.parser
    , Parser.map ParableOfPolygonsClone Gen.Params.ParableOfPolygonsClone.parser
    , Parser.map Pops Gen.Params.Pops.parser
    , Parser.map ScriptaDemo Gen.Params.ScriptaDemo.parser
    , Parser.map SpeedReadDemo Gen.Params.SpeedReadDemo.parser
    , Parser.map VegaLite Gen.Params.VegaLite.parser
    , Parser.map WordleClone Gen.Params.WordleClone.parser
    , Parser.map NotFound Gen.Params.NotFound.parser
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
        BouncingBall ->
            joinAsHref [ "bouncing-ball" ]
    
        ElmUiSvgIssue ->
            joinAsHref [ "elm-ui-svg-issue" ]
    
        Home_ ->
            joinAsHref []
    
        ParableOfPolygonsClone ->
            joinAsHref [ "parable-of-polygons-clone" ]
    
        Pops ->
            joinAsHref [ "pops" ]
    
        ScriptaDemo ->
            joinAsHref [ "scripta-demo" ]
    
        SpeedReadDemo ->
            joinAsHref [ "speed-read-demo" ]
    
        VegaLite ->
            joinAsHref [ "vega-lite" ]
    
        WordleClone ->
            joinAsHref [ "wordle-clone" ]
    
        Stories__ParableOfPolygonsQa ->
            joinAsHref [ "stories", "parable-of-polygons-qa" ]
    
        NotFound ->
            joinAsHref [ "not-found" ]

