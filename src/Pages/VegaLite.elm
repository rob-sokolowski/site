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
            ]
            (elements model)
        ]
    }


elements : Model -> Element Msg
elements model =
    row
        [ htmlAttribute <| HA.id "elm-ui-viz"
        , Border.width 2
        , Border.color UI.palette.black
        , width <| px 200
        , height <| px 400
        ]
        [ E.text "Hello!"
        ]
