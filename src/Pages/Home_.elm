module Pages.Home_ exposing (view)

import Color as C exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body = [ layout [] viewElement ]
    }


viewElement : Element msg
viewElement =
    column [ padding 25, spacing 10 ]
        [ text <| "Hello world, some WIP links.."
        , link [ Font.color blue ]
            { url = "/game-of-life"
            , label = text "Game of Life"
            }
        , link [ Font.color blue ]
            { url = "/animation"
            , label = text "Animation"
            }
        ]


blue =
    Element.rgb 0 0.4 0.7
