module Pages.Sheet exposing (Model, Msg, page)

import Array
import Browser.Events as Events
import Color
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Sheet exposing (Params)
import Json.Decode as Decode
import Page
import Request
import Set exposing (Set)
import Shared
import String
import Task
import Time
import UI
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


type PromptMode
    = Idle
    | PromptInProgress String
    | UserTextSubmitted ( String, ( RowIx, ColumnLabel ) )


type alias Model =
    { sheetIdx : Index
    , sheetColumns : Array.Array ColumnData
    , sheetRowCount : Int
    , keysDown : Set KeyCode
    , selectedCoords : Maybe ( RowIx, ColumnLabel )
    , selectedValue : Maybe CellData
    , promptMode : PromptMode
    , testInput : String
    , lastSubmission : Maybe ( String, ( RowIx, ColumnLabel ) )
    }


prompt2Str : PromptMode -> String
prompt2Str pm =
    case pm of
        Idle ->
            "Idle"

        PromptInProgress str ->
            "prompt thus far: " ++ str

        UserTextSubmitted _ ->
            "text being submitted"


type alias RowNumber =
    Int


type alias Index =
    Array.Array TableIndex


type TableIndex
    = RowIdx RowNumber


type alias RowIx =
    Int


type alias ColumnLabel =
    String


type alias ColumnData =
    { label : ColumnLabel
    , col : List ( RowIx, CellData )
    }


type CellData
    = Empty
    | String_ String
    | Float_ Float
    | Int_ Int
    | Bool_ Bool


cell2Str : CellData -> String
cell2Str cd =
    case cd of
        Empty ->
            -- HACK: single space vs empty str yields elm-ui table styling
            " "

        String_ s ->
            s

        Float_ f ->
            String.fromFloat f

        Int_ i ->
            String.fromInt i

        Bool_ b ->
            case b of
                True ->
                    "TRUE"

                False ->
                    "FALSE"


index2Str : TableIndex -> String
index2Str ti =
    case ti of
        RowIdx ix ->
            String.fromInt ix


init : ( Model, Effect Msg )
init =
    let
        rowCount =
            10

        rowIx =
            Array.initialize rowCount identity

        tableIndex =
            Array.map (\e -> RowIdx e) rowIx

        columnCount =
            7

        labels =
            Array.fromList [ "A", "B", "C", "D", "E", "F", "G" ]

        columns =
            Array.map (\lbl -> ColumnData lbl (List.map (\rix -> ( rix, Float_ 1.23 )) (Array.toList rowIx))) labels
    in
    ( { sheetIdx = tableIndex
      , sheetColumns = columns
      , sheetRowCount = rowCount
      , keysDown = Set.empty
      , selectedCoords = Nothing
      , selectedValue = Nothing
      , promptMode = Idle
      , testInput = ""
      , lastSubmission = Nothing
      }
    , Effect.none
    )



-- UPDATE


getValueAtCoords : Model -> RowIx -> ColumnLabel -> Maybe CellData
getValueAtCoords model rix lbl =
    let
        colList =
            Array.toList model.sheetColumns

        targetLbl =
            List.filter (\e -> e.label == lbl) colList

        targetCol : Maybe ColumnData
        targetCol =
            case targetLbl of
                [] ->
                    Nothing

                [ x ] ->
                    Just x

                x :: xs ->
                    Nothing

        targetRow =
            case targetCol of
                Nothing ->
                    Nothing

                Just colData ->
                    let
                        targetRow_ =
                            List.filter (\( rix_, cd ) -> rix_ == rix) colData.col
                    in
                    case targetRow_ of
                        [] ->
                            Nothing

                        [ x_ ] ->
                            Just x_

                        x :: xs ->
                            Nothing
    in
    case targetRow of
        Just ( rix__, cd_ ) ->
            Just cd_

        Nothing ->
            Nothing


type alias KeyCode =
    String


type alias RawPromptSubmission =
    String


type Msg
    = KeyWentDown KeyCode
    | KeyReleased KeyCode
    | ClickedCell ( RowIx, ColumnLabel )
    | PromptInputChanged String
    | TestInputTxtChanged String
    | UserClickedCommit ( RawPromptSubmission, ( RowIx, ColumnLabel ) )



--| Task_This
--| NewTime Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        KeyWentDown code ->
            let
                newKeys =
                    Set.insert code model.keysDown

                newPromptMode =
                    case model.selectedCoords of
                        Nothing ->
                            Idle

                        Just ( rix, lbl ) ->
                            case model.promptMode of
                                Idle ->
                                    if code == "Enter" then
                                        PromptInProgress ""

                                    else
                                        Idle

                                PromptInProgress v ->
                                    if code == "Enter" then
                                        UserTextSubmitted ( model.testInput, ( rix, lbl ) )

                                    else
                                        PromptInProgress model.testInput

                                UserTextSubmitted _ ->
                                    -- TODO:
                                    Idle
            in
            ( { model
                | keysDown = newKeys
                , promptMode = newPromptMode
                , lastSubmission =
                    case newPromptMode of
                        Idle ->
                            Nothing

                        PromptInProgress _ ->
                            Nothing

                        UserTextSubmitted sub ->
                            Just sub
              }
            , Effect.none
            )

        KeyReleased code ->
            let
                newKeys =
                    Set.remove code model.keysDown
            in
            ( { model | keysDown = newKeys }, Effect.none )

        ClickedCell ( rix, lbl ) ->
            let
                selectedValue =
                    getValueAtCoords model rix lbl
            in
            ( { model
                | selectedCoords = Just ( rix, lbl )
                , selectedValue = selectedValue
              }
            , Effect.none
            )

        PromptInputChanged newStr ->
            case model.promptMode of
                Idle ->
                    ( model, Effect.none )

                UserTextSubmitted _ ->
                    -- TODO: Submit stuff
                    ( model, Effect.none )

                PromptInProgress _ ->
                    ( { model
                        | promptMode = PromptInProgress newStr
                      }
                    , Effect.none
                    )

        TestInputTxtChanged newStr ->
            ( { model | testInput = newStr }, Effect.none )

        UserClickedCommit ( rawSub, ( rix, lbl ) ) ->
            let
                newModel =
                    setCellValue ( rawSub, ( rix, lbl ) )
            in
            ( newModel, Effect.none )


setCellValue : Model -> Model
setCellValue model =
    model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyWentDown keyDecoder)
        , Events.onKeyUp (Decode.map KeyReleased keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- VIEW


view : Model -> View Msg
view model =
    { title = "Sheet Demo"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (elements model)
        ]
    }


sheet : Model -> Element Msg
sheet model =
    let
        viewSheetIndex : Index -> Element Msg
        viewSheetIndex ix =
            let
                ix_ =
                    Array.toList ix
            in
            E.table []
                { data = ix_
                , columns =
                    [ { header = E.text " "
                      , width = px 30
                      , view =
                            \r ->
                                el
                                    [ Border.color UI.palette.darkishGrey
                                    , Border.width 1
                                    , Background.color UI.palette.lightGrey
                                    ]
                                    (el
                                        [ centerX
                                        , paddingEach { top = 1, bottom = 1, left = 0, right = 0 }
                                        ]
                                     <|
                                        E.text <|
                                            index2Str r
                                    )
                      }
                    ]
                }

        viewSheetColumns : ColumnData -> Element Msg
        viewSheetColumns column =
            let
                cellAttrs : RowIx -> CellData -> List (Attribute Msg)
                cellAttrs rix cd =
                    let
                        shouldHighlightCell : Bool
                        shouldHighlightCell =
                            case model.selectedCoords of
                                Nothing ->
                                    False

                                Just ( rix_, lbl_ ) ->
                                    (rix_ == rix) && (lbl_ == column.label)

                        borderWidth =
                            case shouldHighlightCell of
                                False ->
                                    1

                                True ->
                                    3

                        borderColor =
                            case shouldHighlightCell of
                                False ->
                                    UI.palette.lightGrey

                                True ->
                                    UI.palette.lightBlue
                    in
                    [ Border.color borderColor
                    , Border.width borderWidth
                    , onClick <| ClickedCell ( rix, column.label )

                    --, paddingEach { top = 1, left = 0, right = 0, bottom = 1 }
                    ]

                cellContentAttrs : CellData -> List (Attribute Msg)
                cellContentAttrs cd =
                    let
                        alignment =
                            case cd of
                                Empty ->
                                    centerX

                                String_ _ ->
                                    alignLeft

                                Bool_ _ ->
                                    centerX

                                Float_ _ ->
                                    alignRight

                                Int_ _ ->
                                    alignRight
                    in
                    [ alignment
                    , paddingEach { top = 1, left = 0, right = 0, bottom = 1 }
                    ]

                promptElement : RowIx -> PromptMode -> Element Msg
                promptElement rix pm =
                    let
                        isTargetCell : Bool
                        isTargetCell =
                            case model.selectedCoords of
                                Nothing ->
                                    False

                                Just ( rix_, lbl_ ) ->
                                    (rix_ == rix) && (lbl_ == column.label)
                    in
                    case pm of
                        Idle ->
                            none

                        PromptInProgress _ ->
                            case isTargetCell of
                                True ->
                                    el
                                        [ moveDown 25
                                        , width <| px 50
                                        , height <| px 50
                                        , centerX

                                        --, Background.color color.blue
                                        ]
                                        (Input.text []
                                            { text = model.testInput
                                            , onChange = TestInputTxtChanged
                                            , label = Input.labelHidden ""
                                            , placeholder = Nothing
                                            }
                                        )

                                False ->
                                    none

                        UserTextSubmitted _ ->
                            none
            in
            E.table
                [ padding 0 ]
                { data = column.col
                , columns =
                    [ { header = E.text column.label
                      , width = px 80
                      , view =
                            \( rix, cellValue ) ->
                                el (cellAttrs rix cellValue)
                                    (el (cellContentAttrs cellValue)
                                        (E.column (cellContentAttrs cellValue)
                                            [ E.text (cell2Str cellValue)
                                            , promptElement rix model.promptMode
                                            ]
                                        )
                                    )
                      }
                    ]
                }
    in
    row [ padding 5 ] <|
        [ viewSheetIndex model.sheetIdx ]
            ++ (Array.toList <|
                    Array.map (\e -> viewSheetColumns e) model.sheetColumns
               )


viewDebugPanel : Model -> Element Msg
viewDebugPanel model =
    let
        keysList =
            Set.toList model.keysDown

        keyString =
            String.join "," keysList

        selectedCoordsStr =
            case model.selectedCoords of
                Nothing ->
                    "Click a cell to select it"

                Just ( rix, lbl ) ->
                    "Selection: (" ++ String.fromInt rix ++ ", " ++ lbl ++ ")"

        selectedValueStr =
            case model.selectedValue of
                Nothing ->
                    "No selected value"

                Just v ->
                    "Value: " ++ cell2Str v

        promptModeStr =
            case model.promptMode of
                Idle ->
                    "prompt is idle"

                PromptInProgress str ->
                    "prompt thus far: " ++ str

                UserTextSubmitted _ ->
                    "just submitted"

        lastSubmissionStr : String
        lastSubmissionStr =
            case model.lastSubmission of
                Nothing ->
                    "Awaiting submission"

                Just ( sub_, ( rix, lbl ) ) ->
                    sub_

        testOffsetBox =
            let
                inputText =
                    case model.promptMode of
                        Idle ->
                            ""

                        PromptInProgress str ->
                            str

                        UserTextSubmitted _ ->
                            ""
            in
            Input.text
                [ moveDown 25
                , width <| px 50
                , height <| px 50
                , centerX
                , Border.width 3
                , Background.color UI.palette.black
                ]
                { text = inputText
                , onChange = PromptInputChanged
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text ""
                }
    in
    column
        [ padding 5
        , Border.color UI.palette.black
        , Border.width 2
        ]
        [ text keyString
        , text selectedCoordsStr
        , text selectedValueStr
        , text promptModeStr
        , text lastSubmissionStr

        --, testOffsetBox
        ]


viewCommitButton : Model -> Element Msg
viewCommitButton model =
    let
        attrs =
            [ Border.width 2
            , Border.rounded 2
            , Border.color UI.palette.darkCharcoal
            ]
    in
    case model.lastSubmission of
        Nothing ->
            Input.button attrs { onPress = Nothing, label = text "commit" }

        Just ls ->
            Input.button attrs { onPress = Just <| UserClickedCommit ls, label = text "commit" }


content : Model -> Element Msg
content model =
    column [ spacing 10, padding 10 ]
        [ sheet model
        , viewCommitButton model
        , viewDebugPanel model
        , Input.text []
            { text = model.testInput
            , onChange = TestInputTxtChanged
            , label = Input.labelHidden ""
            , placeholder = Nothing
            }
        ]


elements : Model -> Element Msg
elements model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , Background.color UI.palette.white
        , Font.size 12
        ]
        [ --header
          content model

        --, footer
        ]
