module Pages.Sheet exposing (Model, Msg, page)

import Array as A
import Browser.Dom
import Browser.Events as Events
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Sheet exposing (Params)
import Html.Attributes as HA
import Json.Decode as Decode
import Page
import Request
import Set exposing (Set)
import Shared
import SheetModel exposing (CellData(..), RawPromptString)
import String
import Task
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


type alias Cell =
    ( CellCoords, CellData )


type ReplayState
    = Paused
    | Playing


type alias TimelineState =
    { currentFrame : Int
    , replayState : ReplayState
    }


type Model
    = SheetMode SheetData
    | TimelineMode SheetData TimelineState


type alias SheetState =
    { sheetIdx : Index
    , sheetColumns : A.Array ColumnData
    , sheetRowCount : Int
    , keysDown : Set KeyCode
    , selectedCell : Maybe Cell
    , promptMode : PromptMode
    , submissionHistory : List RawPrompt
    , timeline : A.Array ( Int, SheetData )
    }


type SheetData
    = SheetData SheetState


type Msg
    = KeyWentDown KeyCode
    | KeyReleased KeyCode
    | ClickedCell ( RowIx, ColumnLabel )
    | PromptInputChanged String
    | PromptSubmitted RawPrompt
    | ManualDom__AttemptFocus String
    | ManualDom__FocusResult (Result Browser.Dom.Error ())
    | EnterTimelineMode SheetData
    | EnterSheetMode SheetData
      -- Timeline stuff:
      -- TODO: Should Msg take in a `model` param?
    | JumpToFirstFrame
    | JumpToFrame Int
    | JumpToLastFrame
    | TogglePauseResume



-- TODO: Can we save sheet state history here too?


type alias RawPrompt =
    ( RawPromptString, ( RowIx, ColumnLabel ) )


type alias RowNumber =
    Int


type alias Index =
    A.Array TableIndex


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
            A.initialize rowCount identity

        tableIndex =
            A.map (\e -> RowIdx e) rowIx

        labels : A.Array Int
        labels =
            A.fromList [ 0, 1, 2, 3, 4, 5, 6 ]

        columns =
            A.map (\lbl -> ColumnData lbl (List.map (\rix -> ( rix, Empty )) (A.toList rowIx))) labels
    in
    ( SheetMode <|
        SheetData
            { sheetIdx = tableIndex
            , sheetColumns = columns
            , sheetRowCount = rowCount
            , keysDown = Set.empty
            , selectedCell = Just <| ( ( 0, 0 ), Empty ) -- TODO: DRY up the ini
            , promptMode = Idle
            , submissionHistory = []
            , timeline = A.fromList []
            }
    , Effect.none
    )



-- UPDATE


getValueAtCoords : SheetData -> RowIx -> ColumnLabel -> Maybe CellData
getValueAtCoords model rix lbl =
    let
        colList =
            A.toList model.sheetColumns

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
update msg model_ =
    case model_ of
        TimelineMode sheetData state ->
            case msg of
                EnterSheetMode modelData_ ->
                    ( SheetMode modelData_, Effect.none )

                JumpToFirstFrame ->
                    let
                        ( newFrame, newSheetData ) =
                            case sheetData of
                                SheetData sheetData_ ->
                                    case A.get 0 sheetData_.timeline of
                                        Nothing ->
                                            ( state.currentFrame, sheetData )

                                        Just ( newFrame, newSheetData ) ->
                                            ( newFrame, newSheetData )

                        newTimelineState =
                            {}
                    in
                    ( model_, Effect.none )

                JumpToFrame _ ->
                    ( model_, Effect.none )

                JumpToLastFrame ->
                    let
                        newModelData : SheetData
                        newModelData =
                            sheetData
                    in
                    ( TimelineMode newModelData state, Effect.none )

                TogglePauseResume ->
                    ( model_, Effect.none )

                _ ->
                    -- TODO: This is a code smell for a refactor.
                    ( TimelineMode sheetData state, Effect.none )

        SheetMode model ->
            case msg of
                EnterSheetMode _ ->
                    -- TODO: These "noOp" branches are a code smell for a refactor
                    -- noOp
                    ( model_, Effect.none )

                JumpToFirstFrame ->
                    -- noOp
                    ( model_, Effect.none )

                JumpToFrame _ ->
                    -- noOp
                    ( model_, Effect.none )

                JumpToLastFrame ->
                    -- noOp
                    ( model_, Effect.none )

                TogglePauseResume ->
                    -- noOp
                    ( model_, Effect.none )

                EnterTimelineMode modelData ->
                    ( TimelineMode modelData
                        { currentFrame = 0
                        , replayState = Paused
                        }
                    , Effect.none
                    )

                KeyWentDown code ->
                    let
                        newKeys =
                            Set.insert code model.keysDown

                        ( newPromptMode, cmdToSend, newSelectedCoords ) =
                            case model.selectedCell of
                                Nothing ->
                                    ( Idle, Cmd.none, Nothing )

                                Just ( ( rix, lbl ), _ ) ->
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

                                        newVal : CellData
                                        newVal =
                                            case getValueAtCoords model newRix_ newLbl_ of
                                                Nothing ->
                                                    Empty

                                                Just v ->
                                                    v
                                    in
                                    case model.promptMode of
                                        Idle ->
                                            if code == "Enter" then
                                                ( PromptInProgress "", send <| ManualDom__AttemptFocus prompt_intput_dom_id, Just ( ( newRix_, newLbl_ ), newVal ) )

                                            else
                                                ( Idle, Cmd.none, Just ( ( newRix_, newLbl_ ), newVal ) )

                                        PromptInProgress v ->
                                            if code == "Enter" then
                                                ( Idle, send <| PromptSubmitted ( v, ( newRix_, newLbl_ ) ), Just ( ( newRix_, newLbl_ ), newVal ) )

                                            else
                                                ( PromptInProgress v, Cmd.none, Just ( ( newRix_, newLbl_ ), newVal ) )
                    in
                    ( SheetMode
                        { model
                            | keysDown = newKeys
                            , promptMode = newPromptMode
                            , selectedCell = newSelectedCoords
                        }
                    , Effect.fromCmd cmdToSend
                    )

                KeyReleased code ->
                    let
                        newKeys =
                            Set.remove code model.keysDown
                    in
                    ( SheetMode { model | keysDown = newKeys }, Effect.none )

                ClickedCell ( rix, lbl ) ->
                    let
                        selectedValue : CellData
                        selectedValue =
                            case getValueAtCoords model rix lbl of
                                Nothing ->
                                    Empty

                                Just v ->
                                    v
                    in
                    ( SheetMode
                        { model
                            | selectedCell = Just ( ( rix, lbl ), selectedValue )
                        }
                    , Effect.none
                    )

                PromptInputChanged newStr ->
                    case model.promptMode of
                        Idle ->
                            ( SheetMode model, Effect.none )

                        PromptInProgress _ ->
                            ( SheetMode
                                { model
                                    | promptMode = PromptInProgress newStr
                                }
                            , Effect.none
                            )

                PromptSubmitted ( rawSub, ( rix, lbl ) ) ->
                    let
                        newSheetCols : A.Array ColumnData
                        newSheetCols =
                            setCellValue model (str2Cell rawSub) rix lbl

                        newHistory : List RawPrompt
                        newHistory =
                            model.submissionHistory ++ [ ( rawSub, ( rix, lbl ) ) ]

                        newSelectedCoords =
                            ( rix + 1, lbl )

                        newSelectedValue =
                            case getValueAtCoords model (rix + 1) lbl of
                                Nothing ->
                                    Empty

                                Just v ->
                                    v

                        newTimeline : A.Array ( Int, Model )
                        newTimeline =
                            A.append model.timeline (A.fromList [ ( A.length model.timeline, SheetMode model ) ])
                    in
                    ( SheetMode
                        { model
                            | sheetColumns = newSheetCols
                            , promptMode = Idle -- TODO: Is this redundant to key input handling?
                            , submissionHistory = newHistory
                            , selectedCell = Just ( newSelectedCoords, newSelectedValue )
                            , timeline = newTimeline
                        }
                    , Effect.none
                    )

                ManualDom__AttemptFocus domId ->
                    ( SheetMode model, Effect.fromCmd (Browser.Dom.focus domId |> Task.attempt ManualDom__FocusResult) )

                ManualDom__FocusResult result ->
                    case result of
                        Err _ ->
                            ( SheetMode model, Effect.none )

                        Ok () ->
                            ( SheetMode model, Effect.none )


setCellValue : SheetData -> CellData -> RowIx -> ColumnLabel -> A.Array ColumnData
setCellValue model val rix lbl =
    let
        col : Maybe ColumnData
        col =
            A.get lbl model.sheetColumns

        row : A.Array CellData
        row =
            case col of
                Just v ->
                    A.map (\( _, cd ) -> cd) (A.fromList v.col)

                Nothing ->
                    A.empty

        row_ : A.Array CellData
        row_ =
            A.set rix val row

        row__ : ColumnData
        row__ =
            let
                r =
                    A.toList row_

                ixs =
                    List.range 0 (List.length r - 1)

                r_ =
                    List.map2 (\e ix -> ( ix, e )) r ixs
            in
            { label = lbl
            , col = r_
            }

        newArr : A.Array ColumnData
        newArr =
            A.set lbl row__ model.sheetColumns
    in
    newArr



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SheetMode _ ->
            Sub.batch
                [ Events.onKeyDown (Decode.map KeyWentDown keyDecoder)
                , Events.onKeyUp (Decode.map KeyReleased keyDecoder)
                ]

        TimelineMode _ _ ->
            Sub.none


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            "Sheet Demo"
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


viewSheet : SheetData -> Element Msg
viewSheet model =
    let
        viewSheetIndex : Index -> Element Msg
        viewSheetIndex ix =
            let
                ix_ =
                    A.toList ix
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
                            case model.selectedCell of
                                Nothing ->
                                    False

                                Just ( ( rix_, lbl_ ), _ ) ->
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

                viewCell : Maybe ( CellCoords, CellData ) -> String -> RowIx -> PromptMode -> Element Msg
                viewCell selectedCoords cellValueAsStr rix_ promptMode =
                    let
                        isTargetCell : Bool
                        isTargetCell =
                            case selectedCoords of
                                Nothing ->
                                    False

                                Just ( ( rix__, lbl_ ), _ ) ->
                                    (rix_ == rix__) && (lbl_ == column.label)
                    in
                    case isTargetCell of
                        True ->
                            case promptMode of
                                Idle ->
                                    E.text cellValueAsStr

                                PromptInProgress v ->
                                    Input.text
                                        [ htmlAttribute <| HA.id prompt_intput_dom_id
                                        , padding 0
                                        , Border.width 0
                                        ]
                                        { text = v
                                        , onChange = PromptInputChanged
                                        , label = Input.labelHidden ""
                                        , placeholder = Nothing
                                        }

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
                                            [ viewCell model.selectedCell (cell2Str cellValue) rix model.promptMode
                                            ]
                                        )
                                    )
                      }
                    ]
                }
    in
    row [ padding 5 ] <|
        [ viewSheetIndex model.sheetIdx ]
            ++ (A.toList <|
                    A.map (\e -> viewSheetColumns e) model.sheetColumns
               )


viewTimelinePanel : Model -> Element Msg
viewTimelinePanel model =
    case model of
        SheetMode modelData ->
            Input.button
                [ Border.color UI.palette.black
                , Border.width 1
                , Border.rounded 4
                , padding 4
                , alignTop
                , Background.color UI.palette.lightGrey
                ]
                { onPress = Just <| EnterTimelineMode modelData
                , label = text "Enter Timeline Mode"
                }

        TimelineMode modelData _ ->
            E.row
                [ padding 5
                , spacing 5
                ]
                [ Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| EnterSheetMode modelData
                    , label = text "Back to Edit Mode"
                    }
                , Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| JumpToFirstFrame
                    , label = text "<|-"
                    }
                , Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| JumpToFrame 0
                    , label = text "<"
                    }
                , Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| TogglePauseResume
                    , label = text "||"
                    }
                , Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| JumpToFrame 0
                    , label = text ">"
                    }
                , Input.button
                    [ Border.color UI.palette.black
                    , Border.width 1
                    , Border.rounded 4
                    , padding 4
                    , alignTop
                    , Background.color UI.palette.lightGrey
                    ]
                    { onPress = Just <| JumpToLastFrame
                    , label = text "-|>"
                    }
                ]


viewDebugPanel : SheetData -> Element Msg
viewDebugPanel model =
    let
        keysList =
            Set.toList model.keysDown

        keyString =
            String.join "," keysList

        selectedCoordsStr =
            case model.selectedCell of
                Nothing ->
                    "Click a cell to select it"

                Just ( ( rix, lbl ), _ ) ->
                    "Selection: (" ++ String.fromInt rix ++ ", " ++ String.fromInt lbl ++ ")"

        selectedValueStr =
            case model.selectedCell of
                Nothing ->
                    "No selected value"

                Just ( _, v ) ->
                    "Value: " ++ cell2Str v

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
        , viewPromptHistory model.submissionHistory
        ]


content : Model -> Element Msg
content model =
    let
        viewInstructions : Element Msg
        viewInstructions =
            text "Click a cell to select it, or use arrow keys to change selection. Then, press <Enter> to propose new a value for a cell, which will be submitted upon pressing <Enter> a second time"
    in
    case model of
        SheetMode modelData ->
            column [ spacing 10, padding 10 ]
                [ viewInstructions
                , viewTimelinePanel model
                , viewSheet modelData
                , row
                    [ spacing 5
                    ]
                    [ viewDebugPanel modelData
                    ]
                ]

        TimelineMode modelData _ ->
            column [ spacing 10, padding 10 ]
                [ viewInstructions
                , viewTimelinePanel model
                , viewSheet modelData
                , row
                    [ spacing 5
                    ]
                    [ viewDebugPanel modelData
                    ]
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


prompt_intput_dom_id : String
prompt_intput_dom_id =
    -- page-scoped, static unique identifier to control focus manually
    "prompt-input-element"
