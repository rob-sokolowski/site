module Pages.WordleClone exposing (Model, Msg, page)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.WordleClone exposing (Params)
import Page
import Palette
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
    { secretWord : Array Char
    , userGuesses : Array2D Char
    , currentGuessIndex : Int
    , currentGuess : Array Char
    , currentLetterIndex : Int
    }


init : ( Model, Effect Msg )
init =
    ( { secretWord = Array.fromList [ 'q', 'u', 'a', 'r', 't' ]
      , userGuesses = Array.empty
      , currentGuess = Array.empty
      , currentGuessIndex = 0
      , currentLetterIndex = 0
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserInputsLetter String
    | UserSubmitsGuess


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserInputsLetter letter ->
            let
                letter_ : Char
                letter_ =
                    case String.uncons letter of
                        Just ( ch, _ ) ->
                            ch

                        Nothing ->
                            ' '

                newCurrentGuess =
                    Array.fromList <| Array.toList model.currentGuess ++ [ letter_ ]
            in
            ( { model
                | currentLetterIndex = model.currentLetterIndex + 1
                , currentGuess = newCurrentGuess
              }
            , Effect.none
            )

        UserSubmitsGuess ->
            let
                updatedUserGuesses : Array2D Char
                updatedUserGuesses =
                    -- TODO: This logic needs testing, is there a good way to do that with elm-spa 'exposing' constraints?
                    let
                        lol : List (List Char)
                        lol =
                            Array2D.toListOfLists model.userGuesses

                        lol_ : List (List Char)
                        lol_ =
                            lol ++ [ Array.toList model.currentGuess ]
                    in
                    Array2D.fromListOfLists lol_
            in
            ( { model
                | currentGuessIndex = model.currentGuessIndex + 1
                , currentGuess = Array.empty
                , currentLetterIndex = 0
                , userGuesses = updatedUserGuesses
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- begin region view


view : Model -> View Msg
view model =
    { title = "Wordle Clone"
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
    column
        [ centerX
        , spacingXY 0 20
        , padding 15
        ]
        [ el [ centerX ]
            (paragraph
                [ Font.size 24
                ]
                [ text "Wordle!" ]
            )
        , viewBoard model
        , Input.button
            [ paddingXY 5 0
            , alignRight
            , Border.rounded 5
            , Border.color Palette.darkCharcoal
            , Border.width 1
            , moveLeft 5
            , moveUp 5
            ]
            { onPress = Just UserSubmitsGuess
            , label =
                el
                    [ alignRight
                    , padding 5
                    , Font.size 16
                    ]
                <|
                    text "Submit"
            }
        ]


currentGuessAt : Model -> Int -> String
currentGuessAt model ix =
    case Array.get ix model.currentGuess of
        Just ch ->
            String.fromChar ch

        Nothing ->
            ""


viewBoard : Model -> Element Msg
viewBoard model =
    let
        rowAttrs =
            [ Border.color Palette.lightGrey
            , Border.width 1
            , centerX
            ]

        viewCell : Int -> Int -> Element Msg
        viewCell rix cix =
            let
                borderColor =
                    if rix == model.currentGuessIndex && cix == model.currentLetterIndex then
                        Palette.blue_light

                    else
                        Palette.lightGrey

                cellAttrs =
                    [ Border.color borderColor
                    , Border.width 1
                    , height (px 75)
                    , width (px 75)
                    ]

                inputElement =
                    if rix == model.currentGuessIndex && cix == model.currentLetterIndex then
                        Input.text [ width fill, height fill, Border.width 0 ]
                            { text = ""
                            , label = Input.labelLeft [] E.none
                            , onChange = UserInputsLetter
                            , placeholder = Nothing
                            }

                    else
                        E.none
            in
            el cellAttrs inputElement
    in
    column
        [ width fill
        , height fill
        , centerX
        ]
        (List.map
            (\rix ->
                row rowAttrs
                    (List.map
                        (\cix ->
                            viewCell rix cix
                        )
                        (List.range 0 4)
                    )
            )
            (List.range 0 4)
        )



-- end region view
