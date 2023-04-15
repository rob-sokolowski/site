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
            , Font.family [ Font.typeface "Open Sans", Font.sansSerif ]
            , Font.size 16
            , width fill
            ]
            elements
        ]
    }


h3Attrs : List (Attribute msg)
h3Attrs =
    [ Font.bold
    , Font.size 16
    ]



-- TODO: Brush up on elm-ui-with-context


elements : Element msg
elements =
    column
        [ width (fill |> maximum 900 |> minimum 250)
        , height fill
        , padding 10
        , spacing 8
        , centerX

        --, Border.width 1
        --, Border.rounded 5
        --, Border.color Palette.darkishGrey
        ]
        [ paragraph []
            [ text "Hello world, I'm" ]
        , paragraph [ Font.bold, Font.size 24, moveRight 5, moveUp 2 ] [ text "Rob Sokolowski" ]
        , paragraph []
            [ text """I enjoy writing code, and thinking about data tooling.
                Below are some of my recreational programming projects. Enjoy!"""
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Fir") ]
        , paragraph []
            [ text """This project serves as a testing ground for prototyping my way towards a high performance tool
             providing your data with a semantic layer, exploratory analytics tooling, business process modeling,
             and counterfactual analyses.
            """ ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "https://fir-sandbox-2.lamdera.app"
                , label = text "Fir"
                }
            , text " (keyboard/cursor devices only!)"
            ]
        , paragraph []
            [ text "Source: two repos, "
            , link [ Font.color blue ]
                { url = "https://github.com/project-fir/fir-sandbox"
                , label = text "frontend"
                }
            , text " and "
            , link [ Font.color blue ]
                { url = "https://github.com/project-fir/api"
                , label = text "backend"
                }
            ]
        , paragraph []
            [ text "Here's a video I made awhile back exploring some ideas: "
            , link [ Font.color blue ]
                { url = "https://youtu.be/n9jGZY3aO6w"
                , label = text "process dag, facts, graphs"
                }
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Hippo") ]
        , paragraph []
            [ text """A blazing fast, easy to use flashcard app. Generate flashcards with """
            , link [ Font.color blue ]
                { url = "https://github.com/jxxcarlson/scripta-compiler"
                , label = text " jxxcarlson's Scripta compiler "
                }
            , text """ which supports both programming and mathematical notations. There is no configuration, no libraries to install, no compilation of
            LaTeX code. I use Hippo regularly for my studying. If you have a sec, please go check out Professor Carlson's """
            , link [ Font.color blue ]
                { url = "https://jxxcarlson.io/"
                , label = text " work"
                }
            , text ". You're in for a treat! There's still plenty of UI work to do for Hippo, and I'd like to make the UI responsive for study sessions on the go."
            ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "https://hippo.lamdera.app"
                , label = text "Hippo"
                }
            ]
        , paragraph []
            [ text "Source code: "
            , link [ Font.color blue ]
                { url = "https://github.com/project-fir/hippo-lamdera"
                , label = text "hippo-lamdera"
                }
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Wordle clone") ]
        , paragraph []
            [ text """This was a fun challenge I coded up during some downtime while on a road trip last summer.
            The rule was I was not allowed to look up anything while coding this. The part that surprised me was having to implement a
             custom keyboard layout. This was necessary to support both touch and keyboard inputs to match the experience
             of the real app. The secret word is static and doesn't change daily, I'm calling that done enough for this demo =)"""
            ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "/wordle-clone"
                , label = text "Wordle"
                }
            ]
        , paragraph []
            [ text "Source code: "
            , link [ Font.color blue ]
                { url = "https://github.com/rob-sokolowski/site/blob/main/src/Pages/WordleClone.elm"
                , label = text "wordle-clone"
                }
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Bouncing ball") ]
        , paragraph []
            [ link [ Font.color blue ]
                { url = "/bouncing-ball"
                , label = text "Bouncing ball"
                }
            , text " (intended for medium to large screens) learning to animate things"
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Speed read demo") ]
        , paragraph []
            [ link [ Font.color blue ]
                { url = "/speed-read-demo"
                , label = text "Speed read demo"
                }
            , text " (intended for small screens)"
            ]
        , text " "
        , paragraph []
            [ text "Snippet 1: "
            , link [ Font.color blue ]
                { url = "/snippets/request-response-modes"
                , label = text "Request Response Modes"
                }
            ]
        , paragraph []
            [ text "Snippet 2: "
            , link [ Font.color blue ]
                { url = "/snippets/inaction-is-an-action"
                , label = text "Inaction is an action"
                }
            ]

        --, row [ paddingXY 10 0 ]
        --    [ text "Snippet 2: "
        --    , link [ Font.color blue ]
        --        { url = "/snippets/ant-marching"
        --        , label = text "Ant Marching"
        --        }
        --    ]
        , paragraph [] [ el h3Attrs (text "(An oldie) - Pops") ]
        , paragraph []
            [ link [ Font.color blue ]
                { url = "/pops"
                , label = text "Pops"
                }
            , text " This one is ambitious. I have much to learn before I can proceed!"
            ]
        ]


blue =
    E.rgb 0 0.4 0.7
