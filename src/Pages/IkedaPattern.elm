module Pages.IkedaPattern exposing (Model, Msg, page)

import Browser.Dom
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.IkedaPattern exposing (Params)
import Page
import Palette exposing (toAvhColor)
import Request
import Shared
import Task
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC exposing (Svg)
import TypedSvg.Types as ST
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update_
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { viewportStatus : ViewportStatus
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { viewportStatus = ViewportUnknown
      }
    , Effect.fromCmd (Task.perform Got_Viewport Browser.Dom.getViewport)
    )



-- UPDATE


type ViewportStatus
    = ViewportUnknown
    | ViewportKnown Browser.Dom.Viewport


type Msg
    = Got_Viewport Browser.Dom.Viewport


update_ : Msg -> Model -> ( Model, Effect Msg )
update_ msg model =
    case model.viewportStatus of
        ViewportUnknown ->
            case msg of
                Got_Viewport viewport ->
                    ( { model | viewportStatus = ViewportKnown viewport }, Effect.none )

        --_ ->
        --    ( model, Effect.none )
        ViewportKnown viewport ->
            update msg ( model, viewport )


update : Msg -> ( Model, Browser.Dom.Viewport ) -> ( Model, Effect Msg )
update msg ( model, viewport ) =
    case msg of
        Got_Viewport viewport_ ->
            ( { model | viewportStatus = ViewportKnown viewport_ }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Ikeda Pattern"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (case model.viewportStatus of
                ViewportUnknown ->
                    E.none

                ViewportKnown viewport ->
                    viewElements ( model, viewport )
            )
        ]
    }


viewElements : ( Model, Browser.Dom.Viewport ) -> Element Msg
viewElements ( model, viewport ) =
    let
        ( w, h ) =
            ( viewport.viewport.width - 10, viewport.viewport.height - 10 )
    in
    el
        [ width (px <| round w)
        , height (px <| round h)
        ]
        (E.html <|
            S.svg
                [ SA.width (ST.px w)
                , SA.height (ST.px h)
                , SA.viewBox 0 0 w h
                ]
                [ S.rect
                    [ SA.x (ST.px 10)
                    , SA.y (ST.px 10)
                    , SA.width (ST.px 20)
                    , SA.height (ST.px 20)
                    , SA.stroke (ST.Paint <| toAvhColor Palette.black)
                    , SA.fill (ST.Paint <| toAvhColor Palette.black)
                    , SA.strokeWidth (ST.px 3)
                    ]
                    []
                ]
        )
