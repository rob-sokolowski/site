module Pages.Stories.ParableOfPolygonsQa exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Stories.ParableOfPolygonsQa exposing (Params)
import Page
import Request
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias ElementId =
    String


type alias Model =
    { numButtonClicks : Int
    , hoveredOnEl : Maybe ElementId
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { numButtonClicks = 0
      , hoveredOnEl = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ClickedButton
    | HoveredOnElement ElementId
    | CancelHovers


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedButton ->
            ( { model | numButtonClicks = model.numButtonClicks + 1 }, Effect.none )

        HoveredOnElement elId ->
            ( { model | hoveredOnEl = Just elId }, Effect.none )

        CancelHovers ->
            ( { model | hoveredOnEl = Nothing }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Stories.ParableOfPolygonsQa"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            , Background.color palette.white
            ]
            (viewElements model)
        ]
    }


viewBasicsPanel : Model -> Element Msg
viewBasicsPanel model =
    textColumn
        [ width
            (fill
                |> maximum pxMax
                |> minimum pxMin
            )
        , centerX
        ]
        [ el [ Font.size 60, Font.bold, centerX ] (E.text "Title font")
        , el [ Font.size 48, centerX ] (E.text "Sub-title font")
        , E.text " "
        , el [ Font.size 20, Font.bold, centerX ] (E.text "Paragraph header font")
        , el [ Font.size 20, centerX ] (E.text "Normal paragraph font")
        , paragraph []
            [ E.text "TODO: Put long, wrapping text here, but also preserve responsiveness\n            "
            ]
        ]


viewDebugInfo : Model -> Element Msg
viewDebugInfo model =
    textColumn
        []
        [ E.text ("Button clicks: " ++ String.fromInt model.numButtonClicks)
        ]


viewElements : Model -> Element Msg
viewElements model =
    row
        [ width fill
        , height fill
        , padding 10
        ]
        [ E.column
            [ width
                (fill
                    |> maximum pxMax
                    |> minimum pxMin
                )
            , height fill
            , centerX
            , Background.color palette.white
            , Border.color palette.black
            , Border.width 1
            , padding 5
            , spacing 10
            ]
            [ viewBasicsPanel model
            , viewControlWidgetPanel model
            ]
        , column []
            [ el [ Font.size 18, Font.bold ] (E.text "Debug Info:")
            , viewDebugInfo model
            ]
        ]


viewControlWidgetPanel : Model -> Element Msg
viewControlWidgetPanel model =
    column
        [ centerX
        , padding 5
        , spacing 5
        ]
        [ el [ Font.bold ] <| E.text "Control widgets:"
        , button
            { onClick = Just ClickedButton
            , displayText = "Yo"
            , id = "button-1"
            , isHoveredOn =
                case model.hoveredOnEl of
                    Nothing ->
                        False

                    Just elId ->
                        elId == "button-1"
            }
        ]



-- begin region: UI components


pxMax =
    1200


pxMin =
    600


type alias Palette =
    { blue : Color
    , darkCharcoal : Color
    , lightBlue : Color
    , lightGrey : Color
    , white : Color
    , black : Color
    , darkishGrey : Color
    , red : Color
    }


palette : Palette
palette =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE3 0xE3 0xE6
    , white = rgb255 0xFF 0xFF 0xFF
    , black = rgb255 0x00 0x00 0x00
    , darkishGrey = rgb255 0xAB 0xAA 0xB2
    , red = rgb255 0xFF 0x12 0x10
    }


type alias ButtonProps msg =
    { id : ElementId
    , isHoveredOn : Bool
    , onClick : Maybe msg
    , displayText : String
    }


animatedEl : Animation -> List (E.Attribute msg) -> Element msg -> Element msg
animatedEl =
    let
        animatedUi =
            Animated.ui
                { behindContent = E.behindContent
                , htmlAttribute = E.htmlAttribute
                , html = E.html
                }
    in
    animatedUi el


button : ButtonProps Msg -> Element Msg
button props =
    let
        stepMs =
            250

        slideUp : Animation
        slideUp =
            Animation.steps
                { startAt = [ P.y 0 ]
                , options = []
                }
                [ Animation.step stepMs [ P.y -5 ]
                ]

        innerEl : Element Msg
        innerEl =
            el
                [ width (px 80)
                , height (px 80)
                , Background.color palette.black
                , centerX
                , centerY
                ]
                E.none
    in
    el
        [ width (px 100)
        , height (px 100)
        , Background.color palette.darkishGrey
        , centerY
        , centerX
        , Events.onMouseEnter (HoveredOnElement props.id)
        , Events.onMouseLeave CancelHovers
        ]
    <|
        if props.isHoveredOn then
            animatedEl slideUp [ centerX, centerY ] innerEl

        else
            innerEl
