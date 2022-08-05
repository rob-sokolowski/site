module Pages.Home_ exposing (view)

import Color as C exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Palette
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
        , Border.color Palette.darkishGrey
        ]
        [ el [ Font.size 20 ] <| text "Hello world, welcome to my website."
        , text " "
        , text <| "Below are various applets I've been working on"

        --, link [ Font.color blue ]
        --    { url = "/animation"
        --    , label = text "Animation"
        --    }
        --, link [ Font.color blue ]
        --    { url = "/game-dev"
        --    , label = text "Game Sandbox"
        --    }
        , row [ paddingXY 10 0 ]
            [ link [ Font.color blue ]
                { url = "/sheet"
                , label = text "Sheet"
                }
            , text " (intended for large screens). And related page, "
            , link [ Font.color blue ]
                { url = "/vega-lite"
                , label = text "Vega lite"
                }
            ]
        , row [ paddingXY 10 0 ]
            [ link [ Font.color blue ]
                { url = "https://hippo.lamdera.app"
                , label = text "Hippo flash cards"
                }
            , text " (intended for medium to large screens)"
            ]
        , row [ paddingXY 10 0 ]
            [ link [ Font.color blue ]
                { url = "/speed-read-demo"
                , label = text "Speed read demo"
                }
            , text " (intended for small screens)"
            ]
        , row [ paddingXY 10 0 ]
            [ link [ Font.color blue ]
                { url = "/wordle-clone"
                , label = text "Wordle"
                }
            , text " (clone)"
            ]
        , row [ paddingXY 10 0 ]
            [ link [ Font.color blue ]
                { url = "/pops"
                , label = text "Pops"
                }
            , text " This one is ambitious. I have much to learn before I can proceed!"
            ]
        ]


blue =
    E.rgb 0 0.4 0.7
