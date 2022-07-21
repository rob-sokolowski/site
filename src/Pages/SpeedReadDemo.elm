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
    = Paused CurrentIndex
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
    in
    { text = A.fromList textList
    , state = Paused 0
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

                        Paused ix ->
                            Playing ix 50
            in
            ( { model | state = newState }, Effect.none )

        PauseSpeedReading ->
            let
                newState =
                    case model.state of
                        Playing ix _ ->
                            Paused ix

                        Paused ix ->
                            Paused ix
            in
            ( { model | state = newState }, Effect.none )

        ChangeFrameMs newFrameMs ->
            let
                newState =
                    case model.state of
                        Paused ix ->
                            -- noop, but if we want to resume at a different speed maybe I want this.
                            Paused ix

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

                        Paused ix ->
                            Paused ix
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
                Paused _ ->
                    -- dummy value.. makes me think there's a better way to do this.
                    toFloat 10

                Playing _ frameMs_ ->
                    toFloat frameMs_
    in
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
                        Paused _ ->
                            ( StartSpeedReading, "|>" )

                        Playing _ _ ->
                            ( PauseSpeedReading, "||" )
            in
            row
                [ width <| px 600
                , height <| px 70
                , Border.color UI.palette.black
                , Border.width 1
                , centerX
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

                        Paused ix ->
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
