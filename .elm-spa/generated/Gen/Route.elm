module Gen.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

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
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = BouncingBall
    | ElmUiSvgIssue
    | Home_
    | IkedaPattern
    | ParableOfPolygonsClone
    | Pops
    | ScriptaDemo
    | SpeedReadDemo
    | WordleClone
    | Stories__AccelerometerTest
    | Stories__IframeTest
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
    , Parser.map IkedaPattern Gen.Params.IkedaPattern.parser
    , Parser.map ParableOfPolygonsClone Gen.Params.ParableOfPolygonsClone.parser
    , Parser.map Pops Gen.Params.Pops.parser
    , Parser.map ScriptaDemo Gen.Params.ScriptaDemo.parser
    , Parser.map SpeedReadDemo Gen.Params.SpeedReadDemo.parser
    , Parser.map WordleClone Gen.Params.WordleClone.parser
    , Parser.map NotFound Gen.Params.NotFound.parser
    , Parser.map Stories__AccelerometerTest Gen.Params.Stories.AccelerometerTest.parser
    , Parser.map Stories__IframeTest Gen.Params.Stories.IframeTest.parser
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
    
        IkedaPattern ->
            joinAsHref [ "ikeda-pattern" ]
    
        ParableOfPolygonsClone ->
            joinAsHref [ "parable-of-polygons-clone" ]
    
        Pops ->
            joinAsHref [ "pops" ]
    
        ScriptaDemo ->
            joinAsHref [ "scripta-demo" ]
    
        SpeedReadDemo ->
            joinAsHref [ "speed-read-demo" ]
    
        WordleClone ->
            joinAsHref [ "wordle-clone" ]
    
        Stories__AccelerometerTest ->
            joinAsHref [ "stories", "accelerometer-test" ]
    
        Stories__IframeTest ->
            joinAsHref [ "stories", "iframe-test" ]
    
        Stories__ParableOfPolygonsQa ->
            joinAsHref [ "stories", "parable-of-polygons-qa" ]
    
        NotFound ->
            joinAsHref [ "not-found" ]

