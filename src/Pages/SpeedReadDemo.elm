module Pages.SpeedReadDemo exposing (Model, Msg, page)

import Array as A
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.SpeedReadDemo exposing (Params)
import Page
import Request
import Shared
import Time
import UI
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


type alias CurrentIndex =
    Int


type alias FrameMs =
    Int


type SpeedReadState
    = Paused CurrentIndex FrameMs
    | Playing CurrentIndex FrameMs


type alias Model =
    { text : A.Array String
    , state : SpeedReadState
    }


freshModel : Model
freshModel =
    let
        textList =
            String.split " " greatGatsbyChapter1

        textList2 =
            List.filter (\e -> not <| String.startsWith "\n" e) textList
    in
    { text = A.fromList textList2
    , state = Paused 0 250
    }


init : ( Model, Effect Msg )
init =
    ( freshModel
    , Effect.none
    )



-- UPDATE


type Msg
    = StartSpeedReading
    | PauseSpeedReading
    | ChangeFrameMs FrameMs
    | Restart
    | FrameTick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        StartSpeedReading ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Playing ix frameMs

                        Paused ix frameMs ->
                            Playing ix frameMs
            in
            ( { model | state = newState }, Effect.none )

        PauseSpeedReading ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Paused ix frameMs

                        Paused ix frameMs ->
                            Paused ix frameMs
            in
            ( { model | state = newState }, Effect.none )

        ChangeFrameMs newFrameMs ->
            let
                newState =
                    case model.state of
                        Paused ix _ ->
                            Paused ix newFrameMs

                        Playing ix _ ->
                            -- throw away current frameMs, use new
                            Playing ix newFrameMs
            in
            ( { model | state = newState }, Effect.none )

        FrameTick _ ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Playing (modBy (A.length model.text) (ix + 1)) frameMs

                        Paused ix newFrameMs ->
                            Paused ix newFrameMs
            in
            ( { model | state = newState }, Effect.none )

        Restart ->
            ( freshModel, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        frameMs : Float
        frameMs =
            case model.state of
                Paused _ frameMs_ ->
                    toFloat frameMs_

                Playing _ frameMs_ ->
                    toFloat frameMs_
    in
    case model.state of
        Paused _ _ ->
            Sub.none

        Playing _ _ ->
            Sub.batch
                [ Time.every frameMs FrameTick
                ]



-- VIEW


elements : Model -> Element Msg
elements model =
    let
        controlPanel : Element Msg
        controlPanel =
            let
                ( buttonCmd, buttonLbl ) =
                    case model.state of
                        Paused _ _ ->
                            ( StartSpeedReading, "    |>    " )

                        Playing _ _ ->
                            ( PauseSpeedReading, "    ||    " )
            in
            column
                [ width fill
                , height <| px 150
                , Border.color UI.palette.black
                , Border.width 1
                , centerX
                , padding 10
                ]
                [ Input.button
                    [ Border.color UI.palette.lightGrey
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , centerX
                    , centerY
                    , Background.color UI.palette.darkishGrey
                    ]
                    { onPress = Just buttonCmd
                    , label = text buttonLbl
                    }
                , Input.slider
                    [ height fill
                    , behindContent <|
                        -- Slider track
                        el
                            [ width fill
                            , height <| px 5
                            , centerY
                            , Background.color UI.palette.blue
                            , Border.rounded 6
                            ]
                            E.none
                    ]
                    { onChange = round >> ChangeFrameMs
                    , label =
                        Input.labelAbove [] <|
                            text "TODO frameMS"
                    , min = 0
                    , max = 100
                    , step = Just 10
                    , value =
                        toFloat
                            (case model.state of
                                Paused ix frameMs ->
                                    frameMs

                                Playing ix frameMs ->
                                    frameMs
                            )
                    , thumb =
                        Input.thumb
                            [ width <| px 60
                            , height <| px 60
                            , Border.width 2
                            , Border.rounded 6
                            , Border.color UI.palette.darkCharcoal
                            , Background.color UI.palette.white
                            ]
                    }
                ]

        promptPanel : Element Msg
        promptPanel =
            let
                message =
                    case model.state of
                        Playing ix _ ->
                            case A.get ix model.text of
                                Nothing ->
                                    "ERROR!"

                                Just v ->
                                    v

                        Paused ix _ ->
                            case A.get ix model.text of
                                Nothing ->
                                    "ERROR!"

                                Just v ->
                                    v
            in
            el [ centerX ] <| text message
    in
    column
        [ width fill
        , height fill
        , Border.color UI.palette.darkishGrey
        , Border.width 1
        , alignTop
        , centerX
        ]
        [ controlPanel
        , promptPanel
        ]


view : Model -> View Msg
view model =
    { title = "Speed Read Demo"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            , centerX
            , padding 10
            ]
            (elements model)
        ]
    }



-- begin region data


greatGatsbyChapter1 =
    """
The whole town is desolate. All the cars have the left rear wheel painted black as a mourning wreath and there's a persistent wail all night along the North Shore."
"How gorgeous! Let's go back, Tom. Tomorrow!" Then she added irrelevantly, "You ought to see the baby."
"I'd like to."
"She's asleep. She's two years old. Haven't you ever seen her?"
"Never."




DOO OVER!!!

"""



-- end region data
