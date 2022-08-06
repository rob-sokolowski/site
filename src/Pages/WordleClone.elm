module Pages.WordleClone exposing (Model, Msg, page)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser.Events as Events
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.WordleClone exposing (Params)
import Json.Decode as JD
import Page
import Palette
import Request
import Shared
import Utils exposing (keyDecoder, send)
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
    , currentLetterIndex : Int
    }


init : ( Model, Effect Msg )
init =
    ( { secretWord = Array.fromList [ 'Q', 'U', 'A', 'R', 'T' ]

      -- TODO: In order to have configurable word lengths and attempt counts, the array must be initialized
      -- according to the desired configuration, and the view must also use that configuration!
      , userGuesses = Array2D.fromListOfLists (List.repeat 5 (List.repeat 5 ' '))
      , currentGuessIndex = 0
      , currentLetterIndex = 0
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserSubmitsGuess
    | KeyWentDown String
    | PressedKeyBox Char
    | PressedBackspace


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        PressedBackspace ->
            let
                nextCurrentIndex : Int
                nextCurrentIndex =
                    Maybe.withDefault 0 <| List.maximum [ 0, model.currentLetterIndex - 1 ]

                nextUserGuesses : Array2D Char
                nextUserGuesses =
                    -- NB: we update the current index still attached to the model, not what the next value will be!
                    Array2D.setValueAt ( model.currentGuessIndex, model.currentLetterIndex ) ' ' model.userGuesses
            in
            ( { model
                | currentLetterIndex = nextCurrentIndex
                , userGuesses = nextUserGuesses
              }
            , Effect.none
            )

        KeyWentDown key ->
            let
                ( letter_, nextLetterIndex ) =
                    if String.length key > 1 then
                        -- Ignore keys whose length is greater than 1, this filters out things like "backspace" and "delete"
                        ( Nothing, model.currentLetterIndex )

                    else
                        case String.uncons key of
                            Just ( ch, _ ) ->
                                case Char.isAlpha ch of
                                    True ->
                                        ( Just <| Char.toUpper ch, model.currentLetterIndex + 1 )

                                    False ->
                                        ( Nothing, model.currentLetterIndex )

                            Nothing ->
                                ( Nothing, model.currentLetterIndex )

                newUserGuesses : Array2D Char
                newUserGuesses =
                    case letter_ of
                        Nothing ->
                            model.userGuesses

                        Just ch ->
                            Array2D.setValueAt ( model.currentGuessIndex, model.currentLetterIndex ) ch model.userGuesses
            in
            ( { model
                | userGuesses = newUserGuesses
                , currentLetterIndex = nextLetterIndex
              }
            , Effect.none
            )

        PressedKeyBox char ->
            let
                newUserGuesses : Array2D Char
                newUserGuesses =
                    Array2D.setValueAt ( model.currentGuessIndex, model.currentLetterIndex ) char model.userGuesses

                nextLetterIndex : Int
                nextLetterIndex =
                    model.currentLetterIndex + 1
            in
            ( { model
                | userGuesses = newUserGuesses
                , currentLetterIndex = nextLetterIndex
              }
            , Effect.none
            )

        UserSubmitsGuess ->
            case model.currentLetterIndex of
                -- TODO: This will need to read configuration when I generalize the app
                5 ->
                    -- 5 is one-beyond, since we incremented "current" letter to arrive in this state
                    ( { model
                        | currentGuessIndex = model.currentGuessIndex + 1
                        , currentLetterIndex = 0
                      }
                    , Effect.fromCmd (send UserSubmitsGuess)
                    )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyDown (JD.map KeyWentDown keyDecoder)
        ]



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
        , viewKeyboard model
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


viewKeyboard : Model -> Element Msg
viewKeyboard model =
    let
        keyBackgroundColor : Char -> Color
        keyBackgroundColor char =
            if False then
                Palette.green_keylime
                -- TODO: if char has a correct guess somewhere

            else if List.member char (Array.toList model.secretWord) then
                Palette.yellow_wordle

            else if List.member char (Array.toList (Array.toList (Array2D.toListOfLists model.userGuesses))) then
                Palette.darkishGrey

            else
                Palette.white

        keyBoxAttrs : Char -> List (Attribute Msg)
        keyBoxAttrs ch =
            [ Border.width 1
            , Border.color Palette.black
            , width fill
            , height fill
            , onClick (PressedKeyBox ch)
            ]

        rowAttrs =
            [ spaceEvenly
            , width fill
            , height fill
            , centerX
            , centerY
            , spacing 10
            ]
    in
    column
        [ width fill
        , height (px 150)
        , spaceEvenly
        , spacing 10
        ]
        [ row rowAttrs (List.map (\l -> el (keyBoxAttrs l) (text <| String.fromChar l)) [ 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P' ])
        , row rowAttrs (List.map (\l -> el (keyBoxAttrs l) (text <| String.fromChar l)) [ 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L' ])
        , row rowAttrs
            ([ el
                [ Font.size 10
                , Font.bold
                , Border.width 1
                , Border.color Palette.black
                , height fill
                , width fill
                , onClick UserSubmitsGuess
                ]
                (text "ENTER")
             ]
                ++ List.map (\l -> el (keyBoxAttrs l) (text <| String.fromChar l)) [ 'Z', 'X', 'C', 'V', 'B', 'N', 'M' ]
                ++ [ el [ onClick PressedBackspace ] (text "<x|") ]
            )
        ]


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

                targetChar : Maybe Char
                targetChar =
                    Array2D.getValueAt ( rix, cix ) model.userGuesses

                backgroundColor =
                    if rix >= model.currentGuessIndex then
                        Palette.white

                    else if targetChar == Array.get cix model.secretWord then
                        Palette.green_keylime

                    else if Array.length (Array.filter (\ch -> Just ch == targetChar) model.secretWord) > 0 then
                        Palette.yellow_wordle

                    else
                        Palette.darkishGrey

                cellAttrs =
                    [ Border.color borderColor
                    , Background.color backgroundColor
                    , Border.width 1
                    , height (px 75)
                    , width (px 75)
                    ]

                displayChar : Char
                displayChar =
                    case Array2D.getValueAt ( rix, cix ) model.userGuesses of
                        Just v ->
                            v

                        Nothing ->
                            ' '
            in
            el cellAttrs (el [ centerX, centerY ] <| text (String.fromChar displayChar))
    in
    column
        [ width fill
        , height fill
        , centerX
        ]
        -- TODO: In order to have configurable word lengths and attempt counts, the array must be initialized
        -- according to the desired configuration, and the view must also use that configuration!
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
