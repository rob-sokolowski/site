module Pages.GameDev exposing (Model, Msg, page)

import Browser.Events as Events
import Color
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.GamedevFun exposing (Params)
import Html exposing (Html)
import Json.Decode as Decode
import Page
import Request
import Set
import Shared
import TypedSvg as S exposing (circle, rect, svg)
import TypedSvg.Attributes as SA exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, viewBox, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as TT exposing (Paint(..), px)
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


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { downKeys : List String
    , playerPos : Pos
    , portals : PortalPair
    }


type alias Portal =
    { pos : Pos
    , direction : Direction
    , color : PortalColor
    }


type PortalColor
    = Red
    | Blue


type alias PortalPair =
    { a : Portal
    , b : Portal
    }


type alias Pos =
    { x : Float
    , y : Float
    }


init : ( Model, Effect Msg )
init =
    let
        portalA =
            { pos = { x = 250, y = 250 }, direction = Left, color = Blue }

        portalB =
            { pos = { x = 550, y = 250 }, direction = Left, color = Red }
    in
    ( { downKeys = []
      , playerPos = { x = 125, y = 350 }
      , portals = { a = portalA, b = portalB }
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = KeyDowns KeyCode
    | ClearPressed


type Action
    = MoveX Float
    | MoveY Float


type alias KeyCode =
    String


mapKeyCodeToAction : KeyCode -> Maybe Action
mapKeyCodeToAction code =
    if Set.member code <| Set.fromList [ "w", "W", "ArrowUp" ] then
        Just <| MoveY -10

    else if Set.member code <| Set.fromList [ "s", "S", "ArrowDown" ] then
        Just <| MoveY 10

    else if Set.member code <| Set.fromList [ "a", "A", "ArrowLeft" ] then
        Just <| MoveX -10

    else if Set.member code <| Set.fromList [ "d", "D", "ArrowRight" ] then
        Just <| MoveX 10

    else
        Nothing


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        KeyDowns code ->
            let
                action =
                    mapKeyCodeToAction code

                pos =
                    model.playerPos

                newPlayerPos =
                    case action of
                        Just action_ ->
                            case action_ of
                                MoveX dx ->
                                    { pos | x = pos.x + dx }

                                MoveY dy ->
                                    { pos | y = pos.y + dy }

                        Nothing ->
                            pos
            in
            ( { downKeys = []
              , playerPos = newPlayerPos
              , portals = model.portals
              }
            , Effect.none
            )

        ClearPressed ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDowns keyDecoder)
        , Events.onKeyUp (Decode.succeed ClearPressed)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- VIEW


view : Model -> View Msg
view model =
    { title = "GameDev Fun"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (elements model)
        ]
    }


borderedRow : List (Element Msg) -> Element Msg
borderedRow =
    E.row
        [ E.width E.fill
        , E.height E.fill

        --, scrollbarY
        , E.padding 5
        , E.spacing 5
        , Font.size 16
        , Border.width 2
        , Border.rounded 6
        , Border.color UI.palette.blue
        ]


svgElements : Model -> List (Svg msg)
svgElements model =
    let
        player : Float -> Float -> Svg msg
        player x y =
            circle
                [ SA.cx (TT.px x)
                , SA.cy (TT.px y)
                , SA.r (TT.px 20)
                , SA.fill <| Paint Color.blue
                , SA.strokeWidth (TT.px 2)
                , SA.stroke <| TT.Paint <| Color.rgba 0.8 0 0 0.5
                ]
                []

        portal : Portal -> Svg msg
        portal p =
            let
                color =
                    case p.color of
                        Red ->
                            TT.Paint Color.red

                        Blue ->
                            TT.Paint Color.blue
            in
            rect
                [ SA.x (TT.px p.pos.x)
                , SA.y (TT.px p.pos.y)
                , SA.width (TT.px 75)
                , SA.height (TT.px 150)
                , SA.fill color
                , SA.rx (TT.px 15)
                ]
                []
    in
    [ player model.playerPos.x model.playerPos.y
    , portal model.portals.a
    , portal model.portals.b
    ]


content : Model -> Element Msg
content model =
    let
        viewPortWidth =
            1000

        viewPortHeight =
            700

        svgLayout : { width : Float, height : Float } -> Html msg
        svgLayout viewPort =
            svg [ viewBox 0 0 viewPort.width viewPort.height ] <| svgElements model
    in
    column
        [ E.width <| E.px viewPortWidth
        , E.height <| E.px viewPortHeight
        , centerX
        , centerY
        , Background.color UI.palette.white
        , Border.width 5
        , Border.color UI.palette.lightBlue
        , Border.rounded 3
        ]
        [ E.text <| "Use the arrow keys to move around"
        , E.html <| svgLayout { height = viewPortHeight, width = viewPortWidth }
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

                    --, spacing 10
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
    <|
        [ header
        , content model
        , footer
        ]
