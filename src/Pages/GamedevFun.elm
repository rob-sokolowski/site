module Pages.GamedevFun exposing (Model, Msg, page)

--import TypedSvg exposing (circle, svg)
--import TypedSvg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)
--import TypedSvg.Core exposing (Svg)
--import TypedSvg.Types exposing (Paint(..), px)

import Color
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.GamedevFun exposing (Params)
import Page
import Request
import Shared
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


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "GameDev Fun"
    , body =
        [ layout
            [ width fill
            , height fill
            ]
            (elements model)
        ]
    }



--svgAsElement : Element Msg
--svgAsElement =
--    layout [] myCircle
--myCircle : Svg msg
--myCircle =
--    circle
--        [ cx (px 100)
--        , cy (px 100)
--        , r (px 30)
--        , fill <| Paint Color.blue
--        , strokeWidth (px 2)
--        , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
--        ]
--        []


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


content : Element msg
content =
    row
        [ width <| px 800
        , height <| px 600
        , centerX
        , centerY
        , Background.color UI.palette.white
        ]
        [ text "stuff"
        ]


elements : Model -> Element Msg
elements model =
    let
        header : Element msg
        header =
            row
                [ width fill
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
                [ width <| px 80
                , height <| px 40
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
                [ width fill
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
        [ width fill
        , height fill
        , Background.color UI.palette.darkCharcoal
        , Font.size 12
        ]
    <|
        [ header
        , content
        , footer
        ]
