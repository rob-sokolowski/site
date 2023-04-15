module Pages.Snippets.InactionIsAnAction exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets.InactionIsAnAction exposing (Params)
import Page
import Palette exposing (globalLayoutAttrs, topLevelBlogAttrs)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
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


view : Model -> View Msg
view model =
    { title = "Snippet - Inaction is an Action"
    , body =
        [ layout
            globalLayoutAttrs
            (viewElements model)
        ]
    }


viewElements : Model -> Element Msg
viewElements model =
    column
        (topLevelBlogAttrs
            ++ [ spacing 15
               , padding 10
               ]
        )
        (List.map (\p -> paragraph [] [ p ]) paragraphs1)


paragraphs1 =
    [ E.text """What do I wish I understood when I was younger?
"""
    , E.text """
    Inaction is an action. When you don't act, others will. I am a thoughtful person, but it's not the thought that counts.
    Furthermore, our actions are our choices. And since inaction is an action, we also choose to not act. That..
    is not our choice.
    """
    ]
