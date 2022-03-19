module Pages.Pops exposing (Model, Msg, page)

import Color
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Pops exposing (Params)
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


type SimStatus
    = Running
    | Paused


type alias Model =
    { simStatus : SimStatus
    , world : World
    }


type alias RunConfig =
    { initialPopulationCount : Int
    , numTicks : Int
    }


type alias World =
    { polygons : Dict PolygonId Polygon
    }


type alias PolygonId =
    String


type alias Age =
    Float


type alias Polygon =
    { id : PolygonId
    , gender : Gender
    , color : Color
    , age : Age
    }


type Gender
    = Male
    | Female


type Color
    = Purple
    | Blue
    | Green
    | Pink
    | Gray


resetWorld : World
resetWorld =
    { polygons =
        Dict.fromList
            [ ( "1", { id = "1", gender = Male, color = Purple, age = 0.0 } )
            , ( "2", { id = "2", gender = Male, color = Blue, age = 0.0 } )
            , ( "3", { id = "3", gender = Female, color = Green, age = 0.0 } )
            , ( "4", { id = "4", gender = Male, color = Purple, age = 0.0 } )
            , ( "5", { id = "5", gender = Female, color = Pink, age = 0.0 } )
            , ( "6", { id = "6", gender = Female, color = Blue, age = 0.0 } )
            , ( "7", { id = "7", gender = Female, color = Purple, age = 0.0 } )
            , ( "8", { id = "8", gender = Male, color = Pink, age = 0.0 } )
            , ( "9", { id = "9", gender = Female, color = Gray, age = 0.0 } )
            ]
    }


init : ( Model, Effect Msg )
init =
    ( { simStatus = Paused
      , world = resetWorld
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToggleSimStatus
    | ResetWorld
    | Tick Time.Posix


handleTick : World -> World
handleTick world =
    let
        dt : Float
        dt =
            -- the number of years per tick
            1.0

        polygonAge : PolygonId -> Polygon -> Polygon
        polygonAge pid p =
            { p | age = p.age + dt }

        newPolygons =
            Dict.map polygonAge world.polygons
    in
    { world | polygons = newPolygons }


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleSimStatus ->
            let
                newStatus =
                    case model.simStatus of
                        Paused ->
                            Running

                        Running ->
                            Paused
            in
            ( { model | simStatus = newStatus }, Effect.none )

        Tick _ ->
            let
                newWorld =
                    handleTick model.world
            in
            ( { model | world = newWorld }, Effect.none )

        ResetWorld ->
            let
                newWorld =
                    resetWorld
            in
            ( { model | world = newWorld, simStatus = Paused }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.simStatus of
        Paused ->
            Sub.none

        Running ->
            Time.every 1000 Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pops"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (elements model)
        ]
    }


content : Model -> Element Msg
content model =
    let
        label =
            case model.simStatus of
                Running ->
                    "Pause Sim."

                Paused ->
                    "Resume Sim."

        playPauseButton =
            Input.button
                [ padding 5
                , alignRight
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                , Background.color UI.palette.lightBlue
                ]
                { onPress = Just ToggleSimStatus
                , label = text label
                }

        resetButton =
            Input.button
                [ padding 5
                , alignRight
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                , Background.color UI.palette.lightBlue
                ]
                { onPress = Just ResetWorld
                , label = text "Reset."
                }
    in
    row
        [ padding 10
        , spacing 5
        ]
        [ resetButton
        , playPauseButton
        ]


elements : Model -> Element Msg
elements model =
    let
        header : Element msg
        header =
            row
                [ E.width E.fill
                , padding 10
                , spacing 10
                , Background.color UI.palette.lightGrey
                ]
                [ logo
                , el [ alignRight ] <| text "Header"
                , el [ alignRight ] <| text "Stuff"
                , el [ alignRight ] <| text "Goes"
                , el [ alignRight ] <| text "Here"
                ]

        logo : Element msg
        logo =
            el
                [ E.width <| E.px 80
                , E.height <| E.px 40
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                ]
                (el
                    [ centerX
                    , centerY
                    ]
                 <|
                    text "LOGO"
                )

        footer : Element msg
        footer =
            row
                [ E.width E.fill
                , padding 5
                , Background.color UI.palette.lightGrey
                , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                , Border.color UI.palette.lightGrey
                ]
                [ row
                    [ alignLeft
                    ]
                    [ el [ alignLeft ] <| text "Footer stuff"
                    ]
                ]
    in
    E.column
        [ E.width E.fill
        , E.height E.fill
        , Background.color UI.palette.darkCharcoal
        , Font.size 12
        ]
        [ header
        , content model
        , footer
        ]
