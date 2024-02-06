module Pages.IkedaPattern exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events as Events
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
    | Got_ResizeEvent Int Int


update_ : Msg -> Model -> ( Model, Effect Msg )
update_ msg model =
    case model.viewportStatus of
        ViewportUnknown ->
            case msg of
                Got_Viewport viewport ->
                    ( { model | viewportStatus = ViewportKnown viewport }, Effect.none )

                _ ->
                    ( model, Effect.none )

        --_ ->
        --    ( model, Effect.none )
        ViewportKnown viewport ->
            update msg ( model, viewport )


update : Msg -> ( Model, Browser.Dom.Viewport ) -> ( Model, Effect Msg )
update msg ( model, viewport ) =
    case msg of
        Got_Viewport viewport_ ->
            ( { model | viewportStatus = ViewportKnown viewport_ }, Effect.none )

        Got_ResizeEvent _ _ ->
            ( model
            , Effect.fromCmd <| Task.perform Got_Viewport Browser.Dom.getViewport
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize Got_ResizeEvent
        ]



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
        n =
            50

        dx =
            10

        ( w, h ) =
            ( viewport.viewport.width - 10, viewport.viewport.height - 10 )

        ( vb_w, vb_h ) =
            ( n * dx, n * dx )
    in
    el
        [ width (px <| round w)
        , height (px <| round h)
        , centerX
        , centerY
        , Border.width 1
        , Border.color Palette.black
        ]
        (el [ width shrink, height shrink, Border.color Palette.black, Border.width 1 ] <|
            E.html <|
                S.svg
                    [ SA.width (ST.px w)
                    , SA.height (ST.px h)
                    , SA.viewBox 0 0 vb_w vb_h
                    , SA.color (toAvhColor Palette.white)
                    ]
                    (List.map
                        (\i ->
                            let
                                color =
                                    if modBy 2 i == 0 then
                                        Palette.black

                                    else
                                        Palette.white

                                x =
                                    toFloat i * dx

                                y =
                                    toFloat i * dx
                            in
                            S.rect
                                [ SA.x (ST.px x)
                                , SA.y (ST.px y)
                                , SA.width (ST.px dx)
                                , SA.height (ST.px dx)

                                --, SA.stroke (ST.Paint <| toAvhColor Palette.white)
                                , SA.fill (ST.Paint <| toAvhColor color)
                                , SA.strokeWidth (ST.px 1)
                                ]
                                []
                        )
                        (List.range 0 (n - 1))
                    )
        )
