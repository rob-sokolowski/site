module Pages.Snippets.AntMarching exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events as Events
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets.AntMarching exposing (Params)
import Page
import Palette
import Request
import Shared
import Task
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
    | Ready Browser.Dom.Viewport


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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | renderMode = Ready viewport }, Effect.none )

        GotResizeEvent width height ->
            let
                newViewport : Browser.Dom.Viewport
                newViewport =
                    case model.renderMode of
                        AwaitingViewportInfo ->
                            -- I think this should be rare, chose a reasonable default
                            { scene =
                                { width = 800
                                , height = 600
                                }
                            , viewport =
                                { x = 0
                                , y = 0
                                , width = 800
                                , height = 800
                                }
                            }

                        Ready viewport ->
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
            in
            ( { model | renderMode = Ready newViewport }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- TODO: Grab screen size at startup, and on browser resize events. Thread through view and update.
    Sub.batch [ Events.onResize GotResizeEvent ]



-- VIEW


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



-- begin region: layout math


snippetWidthMax : Int
snippetWidthMax =
    800


snippetWidthMin : Int
snippetWidthMin =
    200


snippetWidth : Browser.Dom.Viewport -> Int
snippetWidth viewport =
    if round viewport.viewport.width > snippetWidthMax then
        snippetWidthMax

    else if round viewport.viewport.width < snippetWidthMin then
        snippetWidthMin

    else
        round viewport.viewport.width


imageWidth : Int -> Int
imageWidth snippetWidth_ =
    snippetWidth_ - 25


imageHeight : Int -> Int
imageHeight imageWidth_ =
    round (0.65 * toFloat imageWidth_)



-- end region: layout math


viewElements : Model -> Element Msg
viewElements model =
    case model.renderMode of
        AwaitingViewportInfo ->
            E.none

        Ready viewport ->
            let
                w : Int
                w =
                    snippetWidth viewport

                imW =
                    imageWidth w

                imH =
                    imageHeight imW
            in
            el
                [ width fill
                , height fill
                , padding 10
                ]
                (column
                    [ width
                        (fill
                            |> maximum snippetWidthMax
                            |> minimum snippetWidthMin
                        )
                    , height fill
                    , centerX
                    , Border.color Palette.black
                    , Border.width 1
                    , spacing 15
                    , padding 10
                    ]
                    [ E.paragraph [] [ E.text intro ]
                    , E.image [ width (px imW), height (px imH), centerX ] { src = antImageUrl, description = "A simply drawn, happy ant" }
                    , E.paragraph [] [ E.text "Some more text" ]
                    ]
                )


antImageUrl =
    "https://storage.cloud.google.com/project-fir-api-production/images/an-ant.png"


intro =
    """
    In this snippet, I will attempt to animate an ant marching. The image below is my inspiration:
    """
