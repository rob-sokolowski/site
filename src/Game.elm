module Game exposing (svgElements)

import Browser.Events exposing (..)
import Color
import Pages.GamedevFun as GameDev exposing (Model)
import TypedSvg exposing (circle, rect, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, viewBox, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


svgElements =
    100



--svgElements : GameDev.Model -> List (Svg msg)
--svgElements model =
--    [ myCircle model.playerPos.x model.playerPos.y
--    , aRect
--    ]
--
--
--myCircle : Float -> Float -> Svg msg
--myCircle x y =
--    circle
--        [ cx (px x)
--        , cy (px y)
--        , r (px 20)
--        , fill <| Paint Color.blue
--        , strokeWidth (px 2)
--        , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
--        ]
--        []
--
--
--aRect : Svg msg
--aRect =
--    rect [ x (px 100), y (px 10), width (px 100), height (px 100), rx (px 15), ry (px 15) ] []
