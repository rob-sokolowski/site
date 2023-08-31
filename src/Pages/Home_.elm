module Pages.Home_ exposing (view)

import Color as C exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Palette exposing (globalLayoutAttrs, topLevelBlogAttrs)
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        [ layout
            globalLayoutAttrs
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
        (topLevelBlogAttrs
            ++ [ padding 10
               , spacing 8
               ]
        )
        [ paragraph []
            [ text "Hello world, welcome to my website."
            , text """I enjoy writing code, and thinking about data tooling.
                Below are some of my recreational programming projects. Enjoy!"""
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Fir") ]
        , paragraph []
            [ text """This is my primary side project. It currently consists of several prototypes exploring ideas for a
             semantic layer, business-process modeling, and counterfactual analyses.
            """ ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "https://fir.dev"
                , label = text "Fir"
                }
            , text " (keyboard/cursor devices only!)"
            ]
        , paragraph []
            [ text "90 second demo of chart-building feature: "
            , link [ Font.color blue ]
                { url = "https://youtu.be/Z2PwBVfGCjQ"
                , label = text "Chart builder demo"
                }
            ]
        , paragraph []
            [ text "3 minute clip, describing my longer-term vision: "
            , link [ Font.color blue ]
                { url = "https://youtu.be/n9jGZY3aO6w"
                , label = text "process dag, facts, graphs"
                }
            ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Hippo") ]
        , paragraph []
            [ text """Yet another flashcard app. Generate flashcards with """
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
            , text """. You're in for a treat! There's still plenty of UI work to do for Hippo. I find its basic usage
            confusing, there's no tutorial / how-to, and I'd like to make it responsive for study sessions on the go.
            """
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
            [ text """
            I was nerd-sniped by discussion in Recurse Center's community chat one morning about animating a bouncing
            ball. This implementation is scrappy, using SVG shapes and Elm's built-in `Tick.Every` task to fake animation.
            There is no hardware acceleration / nor use of key-frames, so performance on small devices is poor. I still had
            fun hacking this one.
            """
            ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "/bouncing-ball"
                , label = text "Bouncing ball"
                }
            ]
        , paragraph []
            [ text "Source code: "
            , link [ Font.color blue ]
                { url = "https://github.com/rob-sokolowski/site/blob/main/src/Pages/BouncingBall.elm"
                , label = text "bouncing-ball"
                }
            ]

        -- TODO: I want to fix the slider ratios before posting this publicly
        --, text " "
        --, paragraph [] [ el h3Attrs (text "Speed read demo") ]
        --, paragraph []
        --    [ link [ Font.color blue ]
        --        { url = "/speed-read-demo"
        --        , label = text "Speed read demo"
        --        }
        --    , text " (intended for small screens)"
        --    ]
        , text " "
        , paragraph [] [ el h3Attrs (text "Pops (an oldie)") ]
        , paragraph [] [ text """
        This was the 2nd iteration on what eventually lead to Fir (linked above). I wrote the first iteration in Python in 2020, and
        unfortunately it's difficult to use / introspect. The lack of accessibility inspired me to start learning front-end development.
        """ ]
        , paragraph []
            [ text "Live demo: "
            , link [ Font.color blue ]
                { url = "/pops"
                , label = text "Pops"
                }
            ]
        , paragraph []
            [ text "Source code: "
            , link [ Font.color blue ]
                { url = "https://github.com/rob-sokolowski/site/blob/main/src/Pages/Pops.elm"
                , label = text "pops"
                }
            ]
        , text " "
        , text " "
        , text " "
        ]


blue =
    E.rgb 0 0.4 0.7
