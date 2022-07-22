module Pages.Home_ exposing (view)

import Color as C exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import UI
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        [ layout
            [ padding 2
            , Font.size 16
            ]
            elements
        ]
    }


elements : Element msg
elements =
    column
        [ width (fill |> maximum 800)
        , height fill
        , padding 25
        , spacing 10
        , centerX
        , Border.width 1
        , Border.rounded 5
        , Border.color UI.palette.darkishGrey
        ]
        [ text <| "Hello world, some WIP links.."

        --, link [ Font.color blue ]
        --    { url = "/animation"
        --    , label = text "Animation"
        --    }
        --, link [ Font.color blue ]
        --    { url = "/game-dev"
        --    , label = text "Game Sandbox"
        --    }
        , row []
            [ link [ Font.color blue ]
                { url = "/sheet"
                , label = text "Sheet"
                }
            , text " (intended for large screens)"
            ]
        , row []
            [ link [ Font.color blue ]
                { url = "/speed-read-demo"
                , label = text "Speed read demo"
                }
            , text " (intended for small screens)"
            ]
        , link [ Font.color blue ]
            { url = "/pops"
            , label = text "Pops"
            }
        ]


blue =
    E.rgb 0 0.4 0.7
