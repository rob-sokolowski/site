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
    , r : Float
    , vx : Float
    , vy : Float
    }


type alias Model =
    { ballPos : Pos
    , g : Float
    , runningState : RunningState
    }


g : Float
g =
    -0.001


defaultPos : Pos
defaultPos =
    { x = 50
    , y = 40
    , r = 30
    , vx = 0.05
    , vy = 0
    }


refreshModel : Model
refreshModel =
    { ballPos = defaultPos
    , g = g
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
                    model.ballPos.vy - g

                vx_ =
                    model.ballPos.vx

                y_ =
                    model.ballPos.y + (vy_ * dt)

                x_ =
                    model.ballPos.x + (vx_ * dt)

                r_ =
                    model.ballPos.r

                newPos =
                    { x = x_
                    , y = y_
                    , r = r_
                    , vy = vy_
                    , vx = vx_
                    }

                newRunningState =
                    case newPos.x >= (1.25 * canvasW) || newPos.x < (-25 * canvasW) of
                        -- we've run out of bounds in the x direction
                        True ->
                            Paused

                        False ->
                            case newPos.y >= (1.5 * canvasH) || newPos.y < (-25 * canvasH) of
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
    in
    row
        [ Border.width 1
        , Border.color Palette.darkishGrey
        , Border.rounded 5
        , Background.color Palette.white
        , width fill
        , height (px 80)
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
        ]


viewSvgViewBox : Model -> Element Msg
viewSvgViewBox model =
    let
        svgNodes : List (Svg Msg)
        svgNodes =
            [ S.circle
                [ SA.cx (ST.px model.ballPos.x)
                , SA.cy (ST.px model.ballPos.y)
                , SA.r (ST.px model.ballPos.r)
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
