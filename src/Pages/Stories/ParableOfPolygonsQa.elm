module Pages.Stories.ParableOfPolygonsQa exposing (Model, Msg, page)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser.Dom
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Stories.ParableOfPolygonsQa exposing (Params)
import Page
import Palette exposing (toAvhColor)
import Request
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Task
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC exposing (Svg)
import TypedSvg.Types as ST
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


type alias Polygon =
    { shape : Shape
    , mood : Mood
    }


type alias Model =
    { numButtonClicks : Int
    , hoveredOnEl : Maybe ElementId
    , viewport : Maybe Browser.Dom.Viewport
    , pageRenderStatus : PageRenderStatus
    , grid : Array2D Polygon

    --, layoutInfo : LayoutInfo
    }


grid3x4 : Array2D Polygon
grid3x4 =
    let
        p1 : Polygon
        p1 =
            { shape = Triangle
            , mood = Happy
            }

        p2 : Polygon
        p2 =
            { shape = Square
            , mood = Mad
            }

        p3 : Polygon
        p3 =
            { shape = Square
            , mood = Mad
            }
    in
    Array2D.fromListOfLists
        [ [ p1, p2, p3, p1 ]
        , [ p3, p1, p2, p3 ]
        , [ p3, p3, p2, p1 ]
        ]


init : Shared.Model -> ( Model, Effect Msg )
init _ =
    ( { numButtonClicks = 0
      , hoveredOnEl = Nothing
      , viewport = Nothing
      , pageRenderStatus = AwaitingDomInfo
      , grid = grid3x4
      }
    , Effect.fromCmd <| Task.perform GotViewport Browser.Dom.getViewport
    )



-- UPDATE


type Msg
    = ClickedButton
    | HoveredOnElement ElementId
    | CancelHovers
    | GotViewport Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedButton ->
            ( { model | numButtonClicks = model.numButtonClicks + 1 }, Effect.none )

        HoveredOnElement elId ->
            ( { model | hoveredOnEl = Just elId }, Effect.none )

        CancelHovers ->
            ( { model | hoveredOnEl = Nothing }, Effect.none )

        GotViewport viewPort ->
            let
                mainPanelWidth : Int
                mainPanelWidth =
                    round <| (viewPort.viewport.width * 0.8)

                mainPanelHeight : Int
                mainPanelHeight =
                    200

                sidePanelWidth : Int
                sidePanelWidth =
                    min 300 (round viewPort.viewport.width - mainPanelWidth - 5)

                canvasPanelWidth : Float
                canvasPanelWidth =
                    toFloat mainPanelWidth - 5

                canvasPanelHeight : Float
                canvasPanelHeight =
                    viewPort.viewport.height - 75

                layout : LayoutInfo
                layout =
                    { mainPanelWidth = mainPanelWidth
                    , mainPanelHeight = mainPanelHeight
                    , sidePanelWidth = sidePanelWidth
                    , canvasElementWidth = canvasPanelWidth
                    , canvasElementHeight = canvasPanelHeight
                    , viewBoxXMin = 0
                    , viewBoxYMin = 0
                    , viewBoxWidth = canvasPanelWidth
                    , viewBoxHeight = canvasPanelHeight
                    }
            in
            ( { model
                | viewport = Just viewPort
                , pageRenderStatus = Ready layout
              }
            , Effect.none
            )



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
            , viewDemoPolygons model
            , viewPolygonGrid model.grid
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
            , displayText = "reset"
            , id = "button-1"
            , widthPx = 100
            , heightPx = 40
            , isHoveredOn =
                case model.hoveredOnEl of
                    Nothing ->
                        False

                    Just elId ->
                        elId == "button-1"
            }
        ]



-- begin region: UI components


type PageRenderStatus
    = AwaitingDomInfo
    | Ready LayoutInfo


type alias LayoutInfo =
    { mainPanelWidth : Int
    , mainPanelHeight : Int
    , sidePanelWidth : Int
    , canvasElementWidth : Float
    , canvasElementHeight : Float
    , viewBoxXMin : Float
    , viewBoxYMin : Float
    , viewBoxWidth : Float
    , viewBoxHeight : Float
    }


pxMax =
    1200


pxMin =
    600


type alias Palette =
    { white : Color
    , black : Color
    , surfaceBlue : Color
    , strokeBlue : Color
    , surfaceYellow : Color
    , strokeYellow : Color
    , lightGrey : Color
    , darkishGrey : Color
    , red : Color
    }


palette : Palette
palette =
    { white = rgb255 0xFF 0xFF 0xFF
    , black = rgb255 0x00 0x00 0x00
    , surfaceBlue = rgb255 0x38 0x67 0xE8
    , strokeBlue = rgb255 0x20 0x3B 0x85
    , surfaceYellow = rgb255 0xFC 0xE4 0x77
    , strokeYellow = rgb255 0xE3 0xDD 0x74
    , lightGrey = rgb255 0xE3 0xE3 0xE6
    , darkishGrey = rgb255 0xAB 0xAA 0xB2
    , red = rgb255 0xFF 0x12 0x10
    }


type alias ButtonProps msg =
    { id : ElementId
    , widthPx : Int
    , heightPx : Int
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


type Shape
    = Triangle
    | Square


type Mood
    = Happy
    | Content
    | Mad


viewPolygonRow : Array Polygon -> Element Msg
viewPolygonRow ps =
    row [] <|
        Array.toList <|
            Array.map (\p -> viewPolygon p.shape p.mood) ps


viewPolygonGrid : Array2D Polygon -> Element Msg
viewPolygonGrid ps2 =
    let
        nRows : Int
        nRows =
            Array2D.rowCount ps2

        range : Array Int
        range =
            Array.fromList (List.range 0 nRows)
    in
    column [] <|
        Array.toList <|
            Array.map (\ix -> viewPolygonRow (Array2D.getRow ix ps2)) range



--Array.map (\row -> viewPolygonRow row) (Array2D.getRow 0 ps2)


viewPolygon : Shape -> Mood -> Element Msg
viewPolygon shape mood =
    let
        polygonPoints : List ( number, number )
        polygonPoints =
            case shape of
                Triangle ->
                    [ ( 0, 100 ), ( 50, 0 ), ( 100, 100 ), ( 0, 100 ) ]

                Square ->
                    [ ( 0, 0 ), ( 100, 0 ), ( 100, 100 ), ( 0, 100 ), ( 0, 0 ) ]

        ( fillColor, strokeColor ) =
            case shape of
                Triangle ->
                    ( toAvhColor palette.surfaceYellow, toAvhColor palette.strokeYellow )

                Square ->
                    ( toAvhColor palette.surfaceBlue, toAvhColor palette.strokeBlue )

        htmlElement : Element Msg
        htmlElement =
            E.html <|
                S.svg
                    [ SA.width (ST.px 150)
                    , SA.height (ST.px 150)
                    , SA.viewBox 0 0 150 150
                    ]
                    [ S.polygon
                        [ --SA.x (ST.px pos.x)
                          --, SA.y (ST.px pos.y)
                          --, SA.width (ST.px w)
                          --, SA.height (ST.px h)
                          --, SA.rx (ST.px rad)
                          SA.points polygonPoints
                        , SA.stroke (ST.Paint strokeColor)
                        , SA.fill (ST.Paint fillColor)
                        , SA.strokeWidth (ST.px 3)
                        ]
                        []
                    ]
    in
    case mood of
        Happy ->
            htmlElement

        Content ->
            htmlElement

        Mad ->
            animatedEl rotationOscillation [] htmlElement


viewDemoPolygons : Model -> Element Msg
viewDemoPolygons model =
    column [ spacing 10, centerX ]
        [ E.text "Happy polygons:   =)"
        , row []
            [ viewPolygon Square Happy
            , viewPolygon Triangle Happy
            ]
        , E.text "Content polygons:   -_-"
        , row []
            [ viewPolygon Square Content
            , viewPolygon Triangle Content
            ]
        , E.text "Mad polygons:    >:("
        , row []
            [ viewPolygon Square Mad
            , viewPolygon Triangle Mad
            ]
        ]


rotationOscillation : Animation
rotationOscillation =
    let
        stepMs =
            300
    in
    Animation.steps
        { startAt = [ P.rotate -15 ]
        , options = [ Animation.loop ]
        }
        [ Animation.wait stepMs
        , Animation.step stepMs [ P.rotate 15 ]
        , Animation.wait stepMs
        , Animation.step stepMs [ P.rotate -15 ]

        --, Animation.wait stepMs
        --, Animation.step stepMs [ P.rotate 60 ]
        --, Animation.wait stepMs
        --, Animation.step stepMs [ P.rotate 0 ]
        ]


button : ButtonProps Msg -> Element Msg
button props =
    let
        stepMs =
            250

        slideUp : Animation
        slideUp =
            Animation.steps
                { startAt = [ P.y 0, P.x 0 ]
                , options = []
                }
                [ Animation.step stepMs [ P.x 3, P.y -5 ]
                ]

        innerEl : Element Msg
        innerEl =
            Input.button
                [ width (px props.widthPx)
                , height (px (props.heightPx - 5))
                , Background.color palette.lightGrey
                , centerX
                , centerY
                , Border.rounded 5
                ]
                { onPress = Just ClickedButton
                , label = el [ centerX, Font.color palette.white, Font.bold ] (E.text props.displayText)
                }
    in
    el
        [ width (px props.widthPx)
        , height (px props.heightPx)
        , Background.color palette.darkishGrey
        , centerY
        , centerX
        , Border.rounded 5
        , Events.onMouseEnter (HoveredOnElement props.id)
        , Events.onMouseLeave CancelHovers
        ]
    <|
        if props.isHoveredOn then
            animatedEl slideUp [ centerX, centerY ] innerEl

        else
            innerEl
