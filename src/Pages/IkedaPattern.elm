module Pages.IkedaPattern exposing (Model, Msg, page)

import Array2D exposing (Array2D)
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
    , pattern : Array2D ( Float, Float, Color )
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { viewportStatus = ViewportUnknown
      , pattern = checkeredPattern n
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


checkeredPattern : Int -> Array2D ( Float, Float, Color )
checkeredPattern n_ =
    Array2D.fromListOfLists <|
        List.map
            (\i ->
                List.map
                    (\j ->
                        if modBy 2 (i + j) == 0 then
                            ( dx * toFloat i, dx * toFloat j, Palette.white )

                        else
                            ( dx * toFloat i, dx * toFloat j, Palette.black )
                    )
                    (List.range 0 (n_ - 1))
            )
            (List.range 0 (n_ - 1))


n =
    50


dx =
    10


viewElements : ( Model, Browser.Dom.Viewport ) -> Element Msg
viewElements ( model, viewport ) =
    let
        _ =
            Debug.log "pattern" model.pattern

        ( w, h ) =
            ( viewport.viewport.width - 10, viewport.viewport.height - 10 )

        ( vb_w, vb_h ) =
            ( n * dx, n * dx )

        square : ( Float, Float, Color ) -> Svg Msg
        square ( x, y, color ) =
            S.rect
                [ SA.x (ST.px x)
                , SA.y (ST.px y)
                , SA.width (ST.px dx)
                , SA.height (ST.px dx)
                , SA.fill (ST.Paint <| toAvhColor color)
                ]
                []
    in
    el
        [ width (px <| round w)
        , height (px <| round h)
        , centerX
        , centerY
        , Border.width 1
        , Border.color Palette.black
        ]
        (E.html <|
            S.svg
                [ SA.width (ST.px w)
                , SA.height (ST.px h)
                , SA.viewBox 0 0 vb_w vb_h
                , SA.color (toAvhColor Palette.white)
                ]
                (List.map (\p -> square p) (Array2D.flattenAsList model.pattern))
        )
