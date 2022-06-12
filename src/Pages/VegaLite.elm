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
import Request
import Shared
import UI
import VegaLite exposing (Spec)
import VegaPort exposing (elmToJS, myVis)
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
            [ E.width E.fill
            , E.height E.fill
            , padding 10
            ]
            (elements model)
        ]
    }


elements : Model -> Element Msg
elements model =
    let
        vegaLiteDiv =
            el
                [ htmlAttribute <| HA.id "elm-ui-viz"
                , Border.color UI.palette.black
                , Border.width 2

                --, width <| px 10
                --, height <| px 10
                ]
                E.none
    in
    column
        [ --htmlAttribute <| HA.id "elm-ui-viz"
          Border.width 2
        , Border.color UI.palette.black
        , padding 10
        , spacing 10

        --, width <| px 200
        --, height <| px 400
        --, alignTop
        ]
        [ el [] (E.text "Hi hi hi!")
        , vegaLiteDiv
        , el [] (E.text "Bye bye bye!")
        ]
