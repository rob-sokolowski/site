module Pages.VegaLite exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.VegaLite exposing (Params)
import Html.Attributes as HA
import Page
import Palette exposing (globalLayoutAttrs)
import PortDefs exposing (elmToJS)
import Request
import Shared
import VegaLite exposing (Position(..), Spec, circle, dataColumn, dataFromColumns, encoding, nums, pName, pQuant, position, title, toVegaLite)
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
    Spec


init : ( Model, Effect Msg )
init =
    ( myVis, Effect.fromCmd <| elmToJS myVis )



-- UPDATE
--


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
    let
        title =
            "VegaLite Demo"
    in
    { title = title
    , body =
        [ layout
            globalLayoutAttrs
            (elements model)
        ]
    }


elements : Model -> Element Msg
elements model =
    let
        vegaLiteDiv =
            el
                [ htmlAttribute <| HA.id "elm-ui-viz"
                , centerX
                , centerY
                ]
                -- Render nothing, this will be overwritten by JS
                E.none
    in
    column
        [ --htmlAttribute <| HA.id "elm-ui-viz"
          Border.width 2
        , Border.color Palette.black
        , padding 10
        , spacing 10
        , width fill
        , height fill
        ]
        [ vegaLiteDiv
        ]


myVis : Spec
myVis =
    let
        data =
            dataFromColumns []
                << dataColumn "x" (nums [ 10, 20, 30 ])

        enc =
            encoding
                << position X [ pName "x", pQuant ]
    in
    toVegaLite
        [ title "Hello, World!" []
        , data []
        , enc []
        , circle []
        ]
