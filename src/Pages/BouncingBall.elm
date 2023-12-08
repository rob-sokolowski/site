module Pages.BouncingBall exposing (Model, Msg, RunningState(..), computeNextPos, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.BouncingBall exposing (Params)
import List.Extra
import Page
import Palette exposing (globalLayoutAttrs, toAvhColor)
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
    , currentFrame : Int
    , hist : List Pos
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
    let
        fps =
            30
    in
    1000 / fps


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
    , currentFrame = 0
    , hist = []
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
    | UserToggledPause
    | TimelineSliderSlidTo Float
    | ProposeIntervention


epsilon : Float
epsilon =
    -- '-3' is  'mm'
    1.0e-4


computeNextPos : Model -> ( Pos, List Pos, Int )
computeNextPos model =
    let
        pos : Pos
        pos =
            model.ballPos

        runState : RunningState
        runState =
            model.runningState

        hist : List Pos
        hist =
            model.hist

        frameNo : Int
        frameNo =
            model.currentFrame
    in
    case runState of
        Paused ->
            ( pos, hist, frameNo )

        Playing ->
            if List.length hist == frameNo then
                let
                    vy_ : Float
                    vy_ =
                        case model.ballPos.y + model.ballPos.ry >= meterMaxHeight of
                            True ->
                                -- we are at boundary, reverse, dampen a bit
                                -1 * bounceDampen * (model.ballPos.vy + (g * (dtMs / 1000.0)))

                            False ->
                                -- we are not near boundary, gravity continues
                                model.ballPos.vy + (g * (dtMs / 1000.0))

                    ry_ : Float
                    ry_ =
                        case meterMaxHeight - model.ballPos.y > (2.0 * r) + epsilon of
                            True ->
                                1.0 * r

                            False ->
                                (r * 0.25) + (meterMaxHeight - model.ballPos.y) / ((meterMaxHeight - model.ballPos.y) + 1.5 * r) * model.ballPos.ry

                    vx_ : Float
                    vx_ =
                        model.ballPos.vx

                    y_ : Float
                    y_ =
                        model.ballPos.y + (vy_ * (dtMs / 1000.0))

                    x_ : Float
                    x_ =
                        model.ballPos.x + (vx_ * (dtMs / 1000.0))

                    rx_ : Float
                    rx_ =
                        model.ballPos.rx

                    nextPos =
                        { x = x_
                        , y = y_
                        , rx = rx_
                        , ry = ry_
                        , vy = vy_
                        , vx = vx_
                        }
                in
                ( nextPos
                , List.append model.hist [ nextPos ]
                , frameNo + 1
                )

            else
                -- TODO: This List.getAt can return Nothing, use non-empty list? But also I don't think I like this
                --       structure at all
                case List.Extra.getAt frameNo hist of
                    Just pos_ ->
                        ( pos_
                        , hist
                        , frameNo + 1
                        )

                    Nothing ->
                        -- TODO: Degenerate case!
                        ( pos, hist, frameNo )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ProposeIntervention ->
            let
                pos : Pos
                pos =
                    model.ballPos

                newPos : Pos
                newPos =
                    { pos | vx = -1 * model.ballPos.vx }

                newHist : List Pos
                newHist =
                    List.take model.currentFrame model.hist
            in
            ( { model
                | ballPos = newPos
                , runningState = Playing
                , hist = newHist
              }
            , Effect.none
            )

        TimelineSliderSlidTo val ->
            ( { model
                | currentFrame = round val
                , ballPos =
                    case List.Extra.getAt (round val) model.hist of
                        Just pos_ ->
                            pos_

                        Nothing ->
                            -- TODO: Generate case! nonempty list??
                            defaultPos
                , runningState = Paused
              }
            , Effect.none
            )

        Tick _ ->
            let
                ( newPos, newHist, newFrame ) =
                    computeNextPos model
            in
            ( { model
                | ballPos = newPos
                , hist = newHist
                , currentFrame = newFrame
              }
            , Effect.none
            )

        UserClickedRefresh ->
            ( refreshModel, Effect.none )

        UserToggledPause ->
            case model.runningState of
                Paused ->
                    ( { model | runningState = Playing }, Effect.none )

                Playing ->
                    ( { model | runningState = Paused }, Effect.none )



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
            globalLayoutAttrs
            (viewElements model)
        ]
    }


viewControlPanel : Model -> Element Msg
viewControlPanel model =
    let
        playPauseText : String
        playPauseText =
            case model.runningState of
                Playing ->
                    "||"

                Paused ->
                    "▶"

        sliderProps : SliderProps Msg
        sliderProps =
            { onSlide = TimelineSliderSlidTo
            , val = toFloat model.currentFrame
            , min = 0.0
            , max = toFloat <| List.length model.hist
            , step = 1.0
            , displayText = Nothing
            }
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
        [ el [ width fill ] (comp_slider sliderProps)
        , row
            [ width shrink
            , alignRight
            , spacing 5
            , paddingXY 5 0
            ]
            [ Input.button [ alignLeft, centerX ]
                { label =
                    el
                        [ Border.width 1
                        , Border.rounded 2
                        , Background.color Palette.lightGrey
                        , Border.color Palette.darkishGrey
                        , padding 2
                        , Font.size 14
                        ]
                        (el [ Font.bold ] <| E.text " ↻ ")
                , onPress = Just UserClickedRefresh
                }
            , Input.button [ alignLeft, centerX ]
                { label =
                    el
                        [ Border.width 1
                        , Border.rounded 2
                        , Background.color Palette.lightGrey
                        , Border.color Palette.darkishGrey
                        , padding 2
                        , Font.size 14
                        , width (px 24)
                        ]
                        (el [ centerX, Font.bold ] <| E.text playPauseText)
                , onPress = Just UserToggledPause
                }
            ]
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
        , E.text <| "current frame: " ++ String.fromInt model.currentFrame
        , E.text <| "running state: " ++ runState2Str model.runningState
        , el
            [ alignBottom
            , centerX
            , Events.onClick ProposeIntervention
            ]
            (text "Intervene!")
        ]


runState2Str : RunningState -> String
runState2Str runState =
    case runState of
        Paused ->
            "Paused"

        Playing ->
            "Playing"



-- begin region: UI components (bouncing-ball specific, for now anyway)
--


type alias SliderProps msg =
    { onSlide : Float -> msg
    , val : Float
    , min : Float
    , max : Float
    , step : Float
    , displayText : Maybe String
    }


comp_slider : SliderProps msg -> Element msg
comp_slider props =
    let
        lbl =
            case props.displayText of
                Nothing ->
                    Input.labelHidden " "

                Just txt ->
                    Input.labelAbove [] (text (txt ++ ": " ++ String.fromFloat props.val))
    in
    Input.slider
        [ height (px 30)
        , width fill
        , behindContent
            (el
                [ width fill
                , height (px 5)
                , centerY
                , Background.color theme.shadowGray
                , Border.rounded 2
                ]
                none
            )
        ]
        { onChange = props.onSlide
        , label = lbl
        , min = props.min
        , max = props.max
        , step = Just props.step
        , value = props.val
        , thumb =
            Input.thumb
                [ width (px 24)
                , height (px 24)
                , Border.rounded 2
                , Border.width 1
                , Border.color theme.shadowGray
                , Background.color theme.lightGray
                ]
        }


theme : BouncingBallColorTheme
theme =
    { white = rgb255 0xFF 0xFF 0xFF
    , lightGray = rgb255 0xC5 0xCD 0xD9
    , shadowGray = rgb255 0x3C 0x3F 0x42
    }


type alias BouncingBallColorTheme =
    { white : Color
    , lightGray : Color
    , shadowGray : Color
    }



-- end region: UI components
