module Pages.ScriptaDemo exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Borer
import Element.Events as Events
import Element.Font as Font
import Gen.Params.ScriptaDemo exposing (Params)
import Page
import Request
import Shared
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
    { title = "Scripta Demo"
    , body = [ layout [] (viewElements model) ]
    }


viewElements : Model -> Element Msg
viewElements model =
    el
        [ width (fill |> minimum 400 |> maximum 1200)
        , height fill
        ]
    <|
        column [ centerX, padding 10 ]
            [ E.text "Scripta!"
            ]
