module Pages.Snippets.AntMarching exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events as Events
import Color
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets.AntMarching exposing (Params)
import Page
import Palette exposing (toAvhColor)
import Request
import Shared
import Task
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


type alias Model =
    { antState : AntState
    , renderMode : RenderMode
    }


init : ( Model, Effect Msg )
init =
    ( { antState = { id = 1, pos = 0.0, mode = Marching }
      , renderMode = AwaitingViewportInfo
      }
    , Effect.fromCmd <|
        Cmd.batch
            [ Task.perform GotViewport Browser.Dom.getViewport
            ]
    )



-- UPDATE


type RenderMode
    = AwaitingViewportInfo
    | Ready Browser.Dom.Viewport LayoutInfo


type alias AntState =
    { id : Int
    , pos : Float
    , mode : AntMode
    }


type AntMode
    = Idle AntMood
    | Marching


type AntMood
    = Happy
    | Restless


type Msg
    = GotViewport Browser.Dom.Viewport
    | GotResizeEvent Int Int



-- begin region: layout math


computeLayoutInfo : Browser.Dom.Viewport -> LayoutInfo
computeLayoutInfo domViewport =
    let
        snippetWidthMax : Int
        snippetWidthMax =
            800

        snippetWidthMin : Int
        snippetWidthMin =
            200

        snippetWidth : Int
        snippetWidth =
            if round domViewport.viewport.width > snippetWidthMax then
                snippetWidthMax

            else if round domViewport.viewport.width < snippetWidthMin then
                snippetWidthMin

            else
                round domViewport.viewport.width

        imageWidth : Int
        imageWidth =
            round (0.7 * toFloat snippetWidth)

        imageHeight : Int
        imageHeight =
            round (0.65 * toFloat imageWidth)

        canvasHeight : Float
        canvasHeight =
            toFloat imageHeight

        canvasWidth : Float
        canvasWidth =
            toFloat imageWidth
    in
    { snippetWidthMax = snippetWidthMax
    , snippetWidthMin = snippetWidthMin
    , imageWidth = imageWidth
    , imageHeight = imageHeight
    , canvasWidth = canvasWidth
    , canvasHeight = canvasHeight
    }



-- end region: layout math


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotViewport domViewport ->
            let
                layoutInfo =
                    computeLayoutInfo domViewport
            in
            ( { model | renderMode = Ready domViewport layoutInfo }, Effect.none )

        GotResizeEvent width height ->
            let
                newViewport : Browser.Dom.Viewport
                newViewport =
                    case model.renderMode of
                        AwaitingViewportInfo ->
                            { scene =
                                { width = toFloat width
                                , height = toFloat height
                                }
                            , viewport =
                                { x = 0
                                , y = 0
                                , width = toFloat width
                                , height = toFloat height
                                }
                            }

                        Ready domViewport _ ->
                            let
                                newScene =
                                    { width = toFloat width
                                    , height = toFloat height
                                    }
                            in
                            { domViewport | scene = newScene }

                newLayoutInfo =
                    computeLayoutInfo newViewport
            in
            ( { model | renderMode = Ready newViewport newLayoutInfo }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onResize GotResizeEvent ]



-- VIEW


type alias LayoutInfo =
    { snippetWidthMax : Int
    , snippetWidthMin : Int
    , imageWidth : Int
    , imageHeight : Int
    , canvasWidth : Float
    , canvasHeight : Float
    }


view : Model -> View Msg
view model =
    { title = "Snippet - Ant Marching"
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Source Sans Pro"
                , Font.sansSerif
                ]
            ]
            (viewElements model)
        ]
    }


viewElements : Model -> Element Msg
viewElements model =
    case model.renderMode of
        AwaitingViewportInfo ->
            E.none

        Ready _ layoutInfo ->
            el
                [ width fill
                , height fill
                , padding 10
                ]
                (column
                    [ width
                        (fill
                            |> maximum layoutInfo.snippetWidthMax
                            |> minimum layoutInfo.snippetWidthMin
                        )
                    , height fill
                    , centerX
                    , Border.color Palette.black
                    , Border.width 1
                    , spacing 15
                    , padding 10
                    ]
                    [ E.paragraph [] [ E.text intro ]
                    , E.image [ width (px layoutInfo.imageWidth), height (px layoutInfo.imageHeight), centerX ] { src = antImageUrl, description = "A simply drawn, happy ant" }
                    , E.paragraph [] [ E.text paragraph2 ]
                    , viewSvgViewBox layoutInfo
                    ]
                )


type alias AntColors =
    { bodyFill : Color.Color
    , bodyStroke : Color.Color
    , eyes : Color.Color
    , pupils : Color.Color
    }


antColors : AntColors
antColors =
    { bodyFill = toAvhColor (rgb255 0x7F 0xD1 0x3B)
    , bodyStroke = toAvhColor (rgb255 0x00 0x00 0x00)
    , eyes = toAvhColor (rgb255 0x7F 0xD1 0x3B)
    , pupils = toAvhColor (rgb255 0x7F 0xD1 0x3B)
    }


viewSvgViewBox : LayoutInfo -> Element Msg
viewSvgViewBox layoutInfo =
    let
        svgFloor : List (Svg Msg)
        svgFloor =
            [ S.line
                [ SA.x1 (ST.px 0)
                , SA.y1 (ST.px layoutInfo.canvasHeight)
                , SA.x2 (ST.px layoutInfo.canvasWidth)
                , SA.y2 (ST.px layoutInfo.canvasHeight)
                , SA.strokeWidth (ST.px 10)
                , SA.stroke <| ST.Paint antColors.bodyStroke
                ]
                []
            ]

        svgNodes : List (Svg Msg)
        svgNodes =
            [ S.ellipse
                [ SA.cx (ST.px 50)
                , SA.cy (ST.px 300)
                , SA.rx (ST.px 20)
                , SA.ry (ST.px 15)
                , SA.fill <| ST.Paint antColors.bodyFill
                , SA.stroke <| ST.Paint antColors.bodyStroke
                ]
                []
            , S.ellipse
                [ SA.cx (ST.px 75)
                , SA.cy (ST.px 300)
                , SA.rx (ST.px 18)
                , SA.ry (ST.px 12)
                , SA.fill <| ST.Paint antColors.bodyFill
                , SA.stroke <| ST.Paint antColors.bodyStroke
                ]
                []
            , S.ellipse
                [ SA.cx (ST.px 105)
                , SA.cy (ST.px 295)
                , SA.rx (ST.px 20)
                , SA.ry (ST.px 18)
                , SA.fill <| ST.Paint antColors.bodyFill
                , SA.stroke <| ST.Paint antColors.bodyStroke
                ]
                []
            ]
    in
    el
        [ Border.width 1
        , Border.color Palette.black
        , width (px <| round layoutInfo.canvasWidth)
        , height (px <| round layoutInfo.canvasHeight)
        , centerX
        ]
    <|
        E.html <|
            S.svg
                [ SA.width (ST.px layoutInfo.canvasWidth)
                , SA.height (ST.px layoutInfo.canvasHeight)
                , SA.viewBox 0 0 layoutInfo.canvasWidth layoutInfo.canvasHeight
                ]
                (svgFloor ++ svgNodes)


antImageUrl =
    "https://storage.googleapis.com/public-assets-fea58g08/images/an-ant.png"


intro =
    """
    In this snippet, I will attempt to animate an ant marching. The image below is my inspiration:
    """


paragraph2 =
    """
    The ant takes breaks from her march. Sometimes feeling happy, sometimes feeling restless.
    """
