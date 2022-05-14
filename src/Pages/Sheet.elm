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
import Element.Input as Input exposing (focusedOnLoad)
import Gen.Params.Sheet exposing (Params)
import Json.Decode as Decode
import Page
import Request
import Set exposing (Set)
import Shared
import SheetModel exposing (CellData(..), RawPromptString)
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


type alias CellCoords =
    ( RowIx, ColumnLabel )


type alias Model =
    { sheetIdx : Index
    , sheetColumns : Array.Array ColumnData
    , sheetRowCount : Int
    , keysDown : Set KeyCode
    , selectedCoords : Maybe CellCoords
    , selectedValue : Maybe CellData
    , promptMode : PromptMode
    , submissionHistory : List RawPrompt
    }


type Msg
    = KeyWentDown KeyCode
    | KeyReleased KeyCode
    | ClickedCell ( RowIx, ColumnLabel )
    | PromptInputChanged String
    | PromptSubmitted RawPrompt



-- TODO: Can we save sheet state history here too?


type alias RawPrompt =
    ( RawPromptString, ( RowIx, ColumnLabel ) )


type alias RowNumber =
    Int


type alias Index =
    Array.Array TableIndex


type TableIndex
    = RowIdx RowNumber


type alias RowIx =
    Int


type alias ColumnLabel =
    Int


type alias ColumnData =
    { label : ColumnLabel
    , col : List ( RowIx, CellData )
    }


str2Cell : String -> CellData
str2Cell s =
    String_ s


cell2Str : CellData -> String
cell2Str cd =
    case cd of
        Empty ->
            -- HACK: single space vs empty str yields elm-ui table styling
            " "

        String_ s ->
            if s == "" then
                -- HACK: single space vs empty str yields elm-ui table styling
                " "

            else
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

        labels : Array.Array Int
        labels =
            Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ]

        columns =
            Array.map (\lbl -> ColumnData lbl (List.map (\rix -> ( rix, Int_ 5 )) (Array.toList rowIx))) labels
    in
    ( { sheetIdx = tableIndex
      , sheetColumns = columns
      , sheetRowCount = rowCount
      , keysDown = Set.empty
      , selectedCoords = Just ( 0, 0 )
      , selectedValue = Nothing
      , promptMode = Idle
      , submissionHistory = []
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



--| Task_This
--| NewTime Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        KeyWentDown code ->
            let
                newKeys =
                    Set.insert code model.keysDown

                ( newPromptMode, cmdToSend, newSelectedCoords ) =
                    case model.selectedCoords of
                        Nothing ->
                            ( Idle, Cmd.none, Nothing )

                        Just ( rix, lbl ) ->
                            let
                                newRix_ =
                                    if code == "ArrowUp" && rix > 0 then
                                        rix - 1

                                    else if code == "ArrowDown" then
                                        rix + 1

                                    else
                                        rix

                                newLbl_ =
                                    if code == "ArrowLeft" && lbl > 0 then
                                        lbl - 1

                                    else if code == "ArrowRight" then
                                        lbl + 1

                                    else
                                        lbl
                            in
                            case model.promptMode of
                                Idle ->
                                    if code == "Enter" then
                                        ( PromptInProgress "", Cmd.none, Just ( newRix_, newLbl_ ) )

                                    else
                                        ( Idle, Cmd.none, Just ( newRix_, newLbl_ ) )

                                PromptInProgress v ->
                                    if code == "Enter" then
                                        ( Idle, send <| PromptSubmitted ( v, ( newRix_, newLbl_ ) ), Just ( newRix_, newLbl_ ) )

                                    else
                                        let
                                            newCode =
                                                -- HACK: only consider "short" keys as potential prompt characters
                                                --       This bypasses the Element.Input focus issue, but at unacceptable
                                                --       costs. Backspace, copy-paste, etc do not work
                                                --       This avoids codes like "ArrowLeft" and "ArrowRight"
                                                --
                                                -- TODO: Figure out how to better control focus of Element.Input
                                                if String.length code == 1 then
                                                    v ++ code

                                                else
                                                    v
                                        in
                                        ( PromptInProgress newCode, Cmd.none, Just ( newRix_, newLbl_ ) )
            in
            ( { model
                | keysDown = newKeys
                , promptMode = newPromptMode
                , selectedCoords = newSelectedCoords
              }
            , Effect.fromCmd cmdToSend
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

                PromptInProgress _ ->
                    ( { model
                        | promptMode = PromptInProgress newStr
                      }
                    , Effect.none
                    )

        PromptSubmitted ( rawSub, ( rix, lbl ) ) ->
            let
                newSheetCols : Array.Array ColumnData
                newSheetCols =
                    setCellValue model (str2Cell rawSub) rix lbl

                newHistory : List RawPrompt
                newHistory =
                    model.submissionHistory ++ [ ( rawSub, ( rix, lbl ) ) ]

                newSelectedCoords =
                    Just ( rix + 1, lbl )
            in
            ( { model
                | sheetColumns = newSheetCols
                , promptMode = Idle -- TODO: Is this redundant to key input handling?
                , submissionHistory = newHistory
                , selectedCoords = newSelectedCoords
              }
            , Effect.none
            )


setCellValue : Model -> CellData -> RowIx -> ColumnLabel -> Array.Array ColumnData
setCellValue model val rix lbl =
    let
        col : Maybe ColumnData
        col =
            Array.get lbl model.sheetColumns

        row : Array.Array CellData
        row =
            case col of
                Just v ->
                    Array.map (\( _, cd ) -> cd) (Array.fromList v.col)

                Nothing ->
                    Array.empty

        row_ : Array.Array CellData
        row_ =
            Array.set rix val row

        row__ : ColumnData
        row__ =
            let
                r =
                    Array.toList row_

                ixs =
                    List.range 0 (List.length r - 1)

                r_ =
                    List.map2 (\e ix -> ( ix, e )) r ixs
            in
            { label = lbl
            , col = r_
            }

        newArr : Array.Array ColumnData
        newArr =
            Array.set lbl row__ model.sheetColumns
    in
    newArr



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


viewSheet : Model -> Element Msg
viewSheet model =
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

                viewCell : Maybe CellCoords -> String -> RowIx -> PromptMode -> Element Msg
                viewCell selectedCoords cellValueAsStr rix_ promptMode =
                    let
                        isTargetCell : Bool
                        isTargetCell =
                            case selectedCoords of
                                Nothing ->
                                    False

                                Just ( rix__, lbl_ ) ->
                                    (rix_ == rix__) && (lbl_ == column.label)
                    in
                    case isTargetCell of
                        True ->
                            case promptMode of
                                Idle ->
                                    E.text cellValueAsStr

                                PromptInProgress v ->
                                    let
                                        vStr =
                                            if v == "" then
                                                " "

                                            else
                                                v
                                    in
                                    el [] <| E.text vStr

                        --[--moveDown 25
                        -- --, width <| px 50
                        -- --, height <| px 50
                        -- --, centerX
                        -- --, Background.color color.blue
                        --]
                        --(Input.text
                        --    [ focusedOnLoad
                        --    , padding 0
                        --    , Border.width 0
                        --    ]
                        --    { text = v
                        --    , onChange = PromptInputChanged
                        --    , label = Input.labelHidden ""
                        --    , placeholder = Nothing
                        --    }
                        --)
                        False ->
                            E.text cellValueAsStr
            in
            E.table
                [ padding 0 ]
                { data = column.col
                , columns =
                    [ { header = E.text <| String.fromInt column.label
                      , width = px 80
                      , view =
                            \( rix, cellValue ) ->
                                el (cellAttrs rix cellValue)
                                    (el (cellContentAttrs cellValue)
                                        (E.column (cellContentAttrs cellValue)
                                            [ viewCell model.selectedCoords (cell2Str cellValue) rix model.promptMode
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
                    "Selection: (" ++ String.fromInt rix ++ ", " ++ String.fromInt lbl ++ ")"

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

        viewPromptHistory : List RawPrompt -> Element Msg
        viewPromptHistory history =
            let
                promptStr : RawPrompt -> Element Msg
                promptStr prompt =
                    case prompt of
                        ( rawStr, ( rix, lbl ) ) ->
                            text <| rawStr ++ "@:(" ++ String.fromInt rix ++ ", " ++ String.fromInt lbl ++ ")"
            in
            column []
                [ text <| "Submission history:"
                , column [ paddingEach { top = 0, left = 5, right = 0, bottom = 0 } ] <| List.map promptStr history
                ]
    in
    column
        [ padding 5
        , Border.color UI.palette.black
        , Border.width 2
        , spacing 5
        ]
        [ text keyString
        , text selectedCoordsStr
        , text selectedValueStr
        , text promptModeStr
        , viewPromptHistory model.submissionHistory
        ]


content : Model -> Element Msg
content model =
    column [ spacing 10, padding 10 ]
        [ viewSheet model
        , viewDebugPanel model
        ]


elements : Model -> Element Msg
elements model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , Background.color UI.palette.white
        , Font.size 12
        ]
        [ content model
        ]



-- utils


send : Msg -> Cmd Msg
send m =
    Task.succeed m
        |> Task.perform identity
