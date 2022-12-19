module Pages.Snippets exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets exposing (Params)
import Page
import Palette
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
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
    { title = "Snippets"
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
    el
        [ width fill
        , height fill
        , padding 10
        ]
        (textColumn
            [ width
                (fill
                    |> maximum 800
                    |> minimum 200
                )
            , height fill
            , centerX
            , Border.color Palette.black
            , Border.width 1
            , spacing 15
            , padding 10
            ]
            (List.map (\p -> paragraph [] [ p ]) paragraphs1)
        )


paragraphs1 =
    [ paragraph []
        [ E.text """To see a list of all snippets, please see the
    """
        , E.link [ Font.color Palette.blue, Font.underline ] { url = "/", label = E.text "homepage" }
        ]
    ]
