module Pages.ElmUiSvgIssue exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.ElmUiSvgIssue exposing (Params)
import Page
import Palette
import Request
import Shared
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
    { hoveredOnFish : Maybe Int
    , mouseOnCard : Bool
    }


init : ( Model, Effect Msg )
init =
    ( { hoveredOnFish = Nothing
      , mouseOnCard = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe
    | MouseEnteredFish Int
    | MouseEnteredCard
    | MouseLeftCard


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )

        MouseEnteredFish fishId ->
            ( { model | hoveredOnFish = Just fishId }, Effect.none )

        MouseEnteredCard ->
            ( { model | mouseOnCard = True }, Effect.none )

        MouseLeftCard ->
            ( { model
                | mouseOnCard = False
                , hoveredOnFish = Nothing
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewElements : Model -> Element Msg
viewElements model =
    let
        viewCard : Element Msg
        viewCard =
            let
                backgroundColor : Int -> Color
                backgroundColor i =
                    case model.hoveredOnFish of
                        Nothing ->
                            Palette.white

                        Just fishId ->
                            case i == fishId of
                                True ->
                                    Palette.darkishGrey

                                False ->
                                    Palette.white
            in
            column
                [ width (px 200)
                , height (px 300)
                , Border.width 1
                , Border.color Palette.black
                , Events.onMouseEnter MouseEnteredCard
                , Events.onMouseLeave MouseLeftCard
                , Background.color
                    (case model.mouseOnCard of
                        True ->
                            Palette.lightGrey

                        False ->
                            Palette.white
                    )
                ]
                [ el [ centerX, centerY, Events.onMouseEnter (MouseEnteredFish 1), Background.color <| backgroundColor 1 ] <| text "One fish"
                , el [ centerX, centerY, Events.onMouseEnter (MouseEnteredFish 2), Background.color <| backgroundColor 2 ] <| text "Two fish"
                , el [ centerX, centerY, Events.onMouseEnter (MouseEnteredFish 3), Background.color <| backgroundColor 3 ] <| text "Three fish"
                , el [ centerX, centerY, Events.onMouseEnter (MouseEnteredFish 4), Background.color <| backgroundColor 4 ] <| text "Four fish"
                ]

        svgNodes : Svg Msg
        svgNodes =
            SC.foreignObject
                [ SA.x (ST.px 50)
                , SA.y (ST.px 100)
                , SA.width (ST.px 250)
                , SA.height (ST.px 450)
                ]
                [ E.layoutWith { options = [ noStaticStyleSheet ] }
                    []
                    viewCard
                ]

        viewCanvas : Element Msg
        viewCanvas =
            E.html <|
                S.svg
                    [ SA.width (ST.px 800)
                    , SA.height (ST.px 600)
                    , SA.viewBox 0 0 800 600
                    ]
                    [ svgNodes ]
    in
    el
        [ width fill
        , height fill
        , padding 5
        , Border.color Palette.darkishGrey
        , Border.width 1
        , Border.rounded 5

        --, Background.color Palette.darkishGrey
        ]
        viewCanvas


view : Model -> View Msg
view model =
    { title = "elm-ui / svg issue"
    , body =
        [ layout
            [ width fill
            , height fill
            , padding 3
            ]
          <|
            viewElements model
        ]
    }
