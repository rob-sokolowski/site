module Pages.Snippets.RequestResponseModes exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets.RequestResponseModes exposing (Params)
import Page
import Palette
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Snippet - On Requests"
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Source Sans Pro"
                , Font.sansSerif
                ]
            ]
            (viewElements model)
        ]
    }


viewElements : Model -> Element Msg
viewElements model =
    el
        [ width fill
        , height fill
        , padding 10
        ]
        (textColumn
            [ width
                (fill
                    |> maximum 800
                    |> minimum 200
                )
            , height fill
            , centerX
            , Border.color Palette.black
            , Border.width 1
            , spacing 15
            , padding 10
            ]
            (List.map (\p -> paragraph [] [ p ]) paragraphs1)
        )


motifColor =
    Palette.blue


paragraphs1 =
    [ E.text """Upon receiving a business oriented request, I've found myself in two modes; Tortoise and Hare.

"""
    , E.text """
    The Hare does the first solution that pops into his mind. And will likely push untested code that day. "Thank you
    for the quick turnaround!", I'll often get. I'll be honest, that feels good, and sometimes it is politically
    advantageous to do this. However, I don't think there's a time I didn't regret approaching work this way. It's
    selfish, it benefits me, it hurts the team. Sit me on a team with another Hare, and shit'll go south real quick. Hares don't scale!
    """
    , E.text """The Tortoise asks himself "What class of things, let's call it X, is this request an instance of? what are a few different
    ways to approach this? how do I test X?". This is a bit more difficult, and on a micro-managed team, requires
    disobedience to implement. However, the times I look back on projects that went well, I worked as a Tortoise
    (sometimes obediently, sometimes not, but still Tortoise-y).
    """
    , E.text """What do I mean by "went well"? I think it comes down to change.
    My Tortoise code has been resilient to change. My Hare code usually is not.
    """
    , E.text """Now, let's talk business. Speed of delivery matters. So what mode of operation should I take on to best
     support the business? Well, will there be change? Let's look at typical outcomes of a business request:
    """
    , paragraph []
        [ E.text """Failure path one. Poor requirements scoping leads to last minute requests as the deadline approaches.
        """
        , el [ Font.color motifColor ] <| E.text """
          This failure results in changing requirements.
          """
        ]
    , paragraph []
        [ E.text """Failure path two. This time work is well-defined, but engineers drop the ball. Product panics, cuts scope to meet
    the deadline.
"""
        , el [ Font.color motifColor ] <| E.text """
    This failure results in changing requirements.
"""
        ]
    , paragraph []
        [ E.text """Success path. Work is well-defined, engineers deliver. The business's investments have paid off.
    They will want more.
    """
        , el [ Font.color motifColor ] <| E.text "This "
        , el [ Font.color motifColor, Font.bold ] <| E.text "success"
        , el [ Font.color motifColor ] <| E.text " results in changing requirements."
        ]
    , paragraph []
        [ el [ Font.color motifColor ] <| E.text "    Your requirements will always change."
        , E.text """
 Embrace the chaos, build toward it.
    Be the Tortoise. It's better code, it's better business.
    """
        ]
    ]
