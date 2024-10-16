module Pages.Stories.IframeTest exposing (Model, Msg, page)


import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html, node)
import Html.Attributes
import Element.Input as Input
import Gen.Params.Stories.IframeTest exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
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
    { title = "iframe test"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (viewElements model)
        ]
    }





viewElements : Model -> Element Msg
viewElements _ =
    el [  width fill, height fill ]
        (html (iframeView))


iframeView : Html Msg
iframeView =
    node "iframe"
        [ Html.Attributes.src "http://hippo.lamdera.app"
        , Html.Attributes.style "border" "none"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        []