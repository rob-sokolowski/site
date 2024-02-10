module Pages.IkedaPattern exposing (Model, Msg, page)

import Array2D exposing (Array2D)
import Basics
import Browser.Dom
import Browser.Events as Events
import Dict
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
import Time
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC exposing (Svg)
import TypedSvg.Types as ST exposing (Transform(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        -- parse query param ?page=
        pageNo : Int
        pageNo =
            Dict.get "page" req.query
                |> Maybe.withDefault ""
                |> String.toInt
                |> Maybe.withDefault 1
    in
    Page.advanced
        { init = init shared pageNo
        , update = update_
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { viewportStatus : ViewportStatus
    , pattern : Array2D ( Float, Float, Color )
    , rotDeg : Float -- rotation to apply, in degrees
    , n : Int
    , pageNo : Int
    }


rotDeg0 =
    0.0


init : Shared.Model -> Int -> ( Model, Effect Msg )
init shared pageNo =
    ( { viewportStatus = ViewportUnknown
      , pattern = checkeredPattern n0
      , rotDeg = rotDeg0
      , n = n0
      , pageNo = pageNo
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
    | Tick Time.Posix


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


tickPage1 : Model -> ( Model, Effect Msg )
tickPage1 model =
    let
        rotDeg : Float
        rotDeg =
            toFloat <| modBy 360 (round <| model.rotDeg + dTheta)

        a : Float
        a =
            20

        n_ : Int
        n_ =
            round <| n0 + (a * Basics.sin (degrees rotDeg))

        -- recompute pattern if n has changed, otherwise don't bother since it'll be the same
        pattern : Array2D ( Basics.Float, Basics.Float, Color )
        pattern =
            if model.n /= n_ then
                checkeredPattern n_

            else
                model.pattern
    in
    ( { model
        | rotDeg = rotDeg
        , n = n_
        , pattern = pattern
      }
    , Effect.none
    )


update : Msg -> ( Model, Browser.Dom.Viewport ) -> ( Model, Effect Msg )
update msg ( model, viewport ) =
    case msg of
        Got_Viewport viewport_ ->
            ( { model | viewportStatus = ViewportKnown viewport_ }, Effect.none )

        Got_ResizeEvent _ _ ->
            ( model
            , Effect.fromCmd <| Task.perform Got_Viewport Browser.Dom.getViewport
            )

        Tick _ ->
            case model.pageNo of
                1 ->
                    tickPage1 model

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS
-- begin region: constants


n0 : number
n0 =
    -- The initial number of squares on one side of the checkered pattern
    30


dx : number
dx =
    -- The number of pixels, in viewbox coordinates of a square (smaller square components of the checkered pattern)
    10


dTheta : Float
dTheta =
    -- The rotation, in degrees, to apply each frame
    1.0


dtMs : number
dtMs =
    -- The target number of milliseconds between frames
    60



-- end region: constants


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize Got_ResizeEvent
        , Time.every dtMs Tick
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


viewElements : ( Model, Browser.Dom.Viewport ) -> Element Msg
viewElements ( model, viewport ) =
    let
        ( w, h ) =
            ( viewport.viewport.width - 10, viewport.viewport.height - 10 )

        ( vb_w, vb_h ) =
            ( toFloat model.n * dx, toFloat model.n * dx )

        square : ( Float, Float, Color ) -> Svg Msg
        square ( x, y, color ) =
            S.rect
                [ SA.x (ST.px <| x)
                , SA.y (ST.px <| y)
                , SA.width (ST.px dx)
                , SA.height (ST.px dx)
                , SA.fill (ST.Paint <| toAvhColor color)
                , SA.stroke (ST.Paint <| toAvhColor color)
                , SA.transform
                    [ Rotate model.rotDeg (vb_w / 2 + dx / 2 + 0.1 * y) (vb_h / 2 + dx / 2 + 0.1 * x)

                    --, Translate 10 11
                    ]
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
