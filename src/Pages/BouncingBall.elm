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
    , runningState : RunningState
    }



-- begin region: constants


g : Float
g =
    let
        slowDownFactor =
            0.5
    in
    -- NB: svg's y-axis is such that down is positive, therefore g is also positive!
    9.8 / slowDownFactor


r : Float
r =
    0.5


dtMs : Float
dtMs =
    20.0


scaleXY : Float
scaleXY =
    75.1


canvasElementWidth : Float
canvasElementWidth =
    meterMaxWidth * scaleXY


canvasElementHeight : Float
canvasElementHeight =
    meterMaxHeight * scaleXY


meterMaxWidth =
    14.0


meterMaxHeight =
    8.0


bounceDampen : Float
bounceDampen =
    0.9


x_0 : Float
x_0 =
    0


y_0 : Float
y_0 =
    2


vx_0 : Float
vx_0 =
    1


vy_0 : Float
vy_0 =
    -2.0



-- end region: constants


defaultPos : Pos
defaultPos =
    { x = x_0
    , y = y_0
    , rx = 1.0 * r
    , ry = 1.0 * r
    , vx = vx_0
    , vy = vy_0
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
    | UserClickedPause
    | TimeTravelToFrame Int


epsilon =
    -- '-3' is  'mm'
    1.0e-4


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                vy_ =
                    case model.ballPos.y + model.ballPos.ry >= meterMaxHeight of
                        True ->
                            -- we are at boundary, reverse, dampen a bit
                            -1 * bounceDampen * (model.ballPos.vy + (g * (dtMs / 1000.0)))

                        False ->
                            -- we are not near boundary, gravity continues
                            model.ballPos.vy + (g * (dtMs / 1000.0))

                ry_ =
                    case meterMaxHeight - model.ballPos.y > (2.0 * r) + epsilon of
                        True ->
                            1.0 * r

                        False ->
                            (r * 0.25) + (meterMaxHeight - model.ballPos.y) / ((meterMaxHeight - model.ballPos.y) + 1.5 * r) * model.ballPos.ry

                vx_ =
                    model.ballPos.vx

                y_ =
                    model.ballPos.y + (vy_ * (dtMs / 1000.0))

                x_ =
                    model.ballPos.x + (vx_ * (dtMs / 1000.0))

                rx_ =
                    model.ballPos.rx

                newPos =
                    { x = x_
                    , y = y_
                    , rx = rx_
                    , ry = ry_
                    , vy = vy_
                    , vx = vx_
                    }

                newRunningState =
                    case newPos.x >= (1.15 * meterMaxWidth) || newPos.x < (-0.15 * meterMaxWidth) of
                        -- we've run out of bounds in the x direction
                        True ->
                            Paused

                        False ->
                            case newPos.y >= (1.15 * meterMaxHeight) || newPos.y < (-0.15 * meterMaxHeight) of
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

        UserClickedPause ->
            ( { model | runningState = Paused }, Effect.none )

        TimeTravelToFrame int ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.runningState of
        Playing ->
            Sub.batch
                [ Time.every dtMs Tick
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
        , height (px 40)
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
                ]
                []
            ]
    in
    el [] <|
        E.html <|
            S.svg
                [ SA.width (ST.px canvasElementWidth)
                , SA.height (ST.px canvasElementHeight)
                , SA.viewBox 0 0 meterMaxWidth meterMaxHeight
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
    row
        [ centerX
        , centerY
        , height (px <| round <| canvasElementHeight + 80.0)
        , width (px <| round <| canvasElementWidth + 340)
        , spacing 5
        ]
        [ column
            [ width fill
            , height fill
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
        , viewDebugPanel model
        ]


viewDebugPanel : Model -> Element Msg
viewDebugPanel model =
    column
        [ width fill
        , height fill
        , clipX
        , clipY
        , Background.color Palette.lightGrey
        , Border.rounded 10
        , Border.color Palette.black
        , Border.width 1
        , padding 10
        , spacing 10
        ]
        [ el [ Font.bold ] <| E.text "Debug info:"
        , E.text <| "g (m/s^2): " ++ String.fromFloat g
        , E.text <| "max X (m): " ++ String.fromFloat meterMaxWidth
        , E.text <| "max Y (m): " ++ String.fromFloat meterMaxHeight
        , E.text <| "pos.x (m): " ++ String.fromFloat model.ballPos.x
        , E.text <| "pos.y (m): " ++ String.fromFloat model.ballPos.y
        , E.text <| "v_x (m/s): " ++ String.fromFloat model.ballPos.vx
        , E.text <| "v_y (m/s): " ++ String.fromFloat model.ballPos.vy
        , E.text <| "r (m): " ++ String.fromFloat r
        , E.text <| "r_x (m): " ++ String.fromFloat model.ballPos.rx
        , E.text <| "r_y (m): " ++ String.fromFloat model.ballPos.ry
        ]
