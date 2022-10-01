module Pages.BouncingBall exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.BouncingBall exposing (Params)
import Page
import Palette exposing (toAvhColor)
import Request
import Shared
import Time
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC exposing (Svg)
import TypedSvg.Types as ST
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


type alias Pos =
    { x : Float
    , y : Float
    , rx : Float
    , ry : Float
    , vx : Float
    , vy : Float
    }


type alias Model =
    { ballPos : Pos
    , g : Float
    , ySquash : Float
    , runningState : RunningState
    }


g : Float
g =
    -0.001


r : Float
r =
    100


bounceDampen : Float
bounceDampen =
    0.93


defaultPos : Pos
defaultPos =
    { x = 50
    , y = 40
    , rx = 1.0 * r
    , ry = 1.0 * r
    , vx = 0.09
    , vy = 0
    }


refreshModel : Model
refreshModel =
    { ballPos = defaultPos
    , g = g
    , ySquash = 1.0
    , runningState = Playing
    }


init : ( Model, Effect Msg )
init =
    ( refreshModel
    , Effect.none
    )


type RunningState
    = Paused
    | Playing



-- UPDATE


type Msg
    = Tick Time.Posix
    | UserClickedRefresh


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                vy_ =
                    case model.ballPos.y + model.ballPos.ry >= canvasH of
                        True ->
                            -- we are at boundary, reverse, dampen a bit
                            -1 * model.ballPos.vy * bounceDampen

                        False ->
                            -- we are not near boundary, gravity continues
                            model.ballPos.vy - g

                ySquash_ =
                    case canvasH - model.ballPos.y > 1.5 * r of
                        True ->
                            1.0

                        False ->
                            (canvasH - model.ballPos.y) / ((canvasH - model.ballPos.y) + 1.5 * r)

                vx_ =
                    model.ballPos.vx

                y_ =
                    model.ballPos.y + (vy_ * dt)

                x_ =
                    model.ballPos.x + (vx_ * dt)

                rx_ =
                    model.ballPos.rx

                ry_ =
                    ySquash_ * r

                newPos =
                    { x = x_
                    , y = y_
                    , rx = rx_
                    , ry = ry_
                    , vy = vy_
                    , vx = vx_
                    }

                newRunningState =
                    case newPos.x >= (1.15 * canvasW) || newPos.x < (-0.15 * canvasW) of
                        -- we've run out of bounds in the x direction
                        True ->
                            Paused

                        False ->
                            case newPos.y >= (1.15 * canvasH) || newPos.y < (-0.15 * canvasH) of
                                -- we've run out of bounds in the y direction
                                True ->
                                    Paused

                                False ->
                                    Playing
            in
            ( { model
                | ballPos = newPos
                , runningState = newRunningState
              }
            , Effect.none
            )

        UserClickedRefresh ->
            ( refreshModel, Effect.none )


dt : Float
dt =
    10


canvasW =
    1000


canvasH =
    650



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.runningState of
        Playing ->
            Sub.batch
                [ Time.every dt Tick
                ]

        Paused ->
            Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Bouncing ball"
    , body =
        [ layout
            [ padding 2
            , Font.size 16
            , Background.color Palette.darkishGrey
            ]
            (viewElements model)
        ]
    }


viewControlPanel : Model -> Element Msg
viewControlPanel model =
    let
        displayMessage : String
        displayMessage =
            case model.runningState of
                Playing ->
                    "Playing.."

                Paused ->
                    "Paused"

        positionMessage : String
        positionMessage =
            "pos: (" ++ String.fromFloat model.ballPos.x ++ ", " ++ String.fromFloat model.ballPos.y ++ ")"
    in
    row
        [ Border.width 1
        , Border.color Palette.darkishGrey
        , Border.rounded 5
        , Background.color Palette.white
        , width fill
        , height (px 80)
        , spacing 10
        ]
        [ el [ centerX ] <| E.text displayMessage
        , Input.button [ alignLeft, centerX ]
            { label =
                el
                    [ Border.width 1
                    , Border.rounded 2
                    , Background.color Palette.lightGrey
                    , Border.color Palette.darkishGrey
                    , padding 2
                    , Font.size 14
                    ]
                    (E.text " ↻ ")
            , onPress = Just UserClickedRefresh
            }
        , el [ alignRight ] <| E.text positionMessage
        ]


viewSvgViewBox : Model -> Element Msg
viewSvgViewBox model =
    let
        svgNodes : List (Svg Msg)
        svgNodes =
            [ S.ellipse
                [ SA.cx (ST.px model.ballPos.x)
                , SA.cy (ST.px model.ballPos.y)
                , SA.rx (ST.px model.ballPos.rx)
                , SA.ry (ST.px model.ballPos.ry)
                , SA.fill <| ST.Paint (toAvhColor Palette.blue)
                , SA.strokeWidth (ST.px 2)
                , SA.stroke <| ST.Paint (toAvhColor Palette.red)
                ]
                []
            ]
    in
    E.html <|
        S.svg
            [ SA.width (ST.px canvasW)
            , SA.height (ST.px canvasH)
            , SA.viewBox 0 0 canvasW canvasH
            ]
            svgNodes


viewCanvas : Model -> Element Msg
viewCanvas model =
    el
        [ centerX
        , centerY
        , Border.rounded 10
        , Background.color Palette.white
        ]
        (viewSvgViewBox model)


viewElements : Model -> Element Msg
viewElements model =
    column
        [ height (px 800)
        , width (px 1200)
        , centerX
        , centerY
        , padding 10
        , Background.color Palette.lightGrey
        , Border.width 1
        , Border.color Palette.black
        , Border.rounded 10

        --, centerX
        ]
        [ viewCanvas model
        , viewControlPanel model
        ]
