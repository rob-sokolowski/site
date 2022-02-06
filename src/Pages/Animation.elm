module Pages.Animation exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Gen.Params.Animation exposing (Params)
import Page
import Request
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
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



-- VIEW


view : Model -> View Msg
view model =
    { title = "Animation Playground"
    , body =
        [ layout [] animatedSquare
        ]
    }


viewElement : Model -> Element Msg
viewElement m =
    row [] [ text <| "This is a test!" ]


animatedSquare : Element msg
animatedSquare =
    animatedEl hoverAnimation
        [ centerX
        , centerY
        , width (px 50)
        , height (px 50)
        , Background.color (rgb 1 0 0)
        ]
        none


hoverAnimation : Animation
hoverAnimation =
    Animation.steps
        { startAt = [ P.y 0 ]
        , options = [ Animation.loop, Animation.easeInOutQuad ]
        }
        [ Animation.step 500 [ P.y 20 ]
        , Animation.step 650 [ P.y 0 ]
        ]



-- Elm UI Animated Helpers


animatedEl : Animation -> List (Attribute msg) -> Element msg -> Element msg
animatedEl =
    -- Element.row or Element.column can be used here too
    animatedUi Element.el


animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }
