module Pages.Sheet exposing (Model, Msg, page)

import Array as A
import Array.Extra as AE
import Array2D as A2 exposing (Array2D, ColIx, RowIx, colCount, fromListOfLists, getCol, rowCount, setValueAt)
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
import SheetModel exposing (Cell, CellCoords, CellElement(..), RawPromptString, SheetData, array2DToSheet, elementAt)
import String exposing (fromInt)
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


type ReplayState
    = Paused
    | Playing


type alias TimelineState =
    { currentFrame : Int
    , replayState : ReplayState
    }


type alias Model =
    { sheetData : SheetData
    , keysDown : Set KeyCode
    , selectedCell : Maybe Cell
    , promptMode : PromptMode
    , submissionHistory : List RawPrompt
    , timeline : A.Array Timeline
    , uiMode : UiMode
    }


type alias CurrentFrame =
    Int


type UiMode
    = SheetEditor
    | TimelineViewer CurrentFrame


type Timeline
    = Timeline Model


type Msg
    = KeyWentDown KeyCode
    | KeyReleased KeyCode
    | ClickedCell CellCoords
    | PromptInputChanged String
    | PromptSubmitted RawPrompt
    | ManualDom__AttemptFocus String
    | ManualDom__FocusResult (Result Browser.Dom.Error ())
    | EnterTimelineViewerMode
    | EnterSheetEditorMode -- TODO: Just toggle UI mode?
      -- Timeline stuff:
      -- TODO: Should Msg take in a `model` param?
    | JumpToFirstFrame
    | JumpToFrame Int
    | JumpToLastFrame
    | TogglePauseResume


type alias RawPrompt =
    ( RawPromptString, ( RowIx, ColumnLabel ) )


type alias ColumnLabel =
    Int


type alias ColumnData =
    { label : ColumnLabel

    -- TODO: IndexedList might be a better fit
    , col : List ( RowIx, CellElement )
    }


str2Cell : String -> CellElement
str2Cell s =
    String_ s


cell2Str : CellElement -> String
cell2Str cd =
    case cd of
        Empty ->
            -- HACK: single space vs empty str yields 'expected' elm-ui table styling
            " "

        String_ s ->
            if s == "" then
                -- HACK: single space vs empty str yields 'expected' elm-ui table styling
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


init : ( Model, Effect Msg )
init =
    let
        data : Array2D CellElement
        data =
            fromListOfLists
                [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                ]

        sheetData : SheetData
        sheetData =
            array2DToSheet data

        -- NB: timeline is recursive, so we save the initial model state in this let expression, and return
        --     a partially updated model containing this one
        model =
            { sheetData = sheetData
            , keysDown = Set.empty
            , selectedCell = Just <| ( ( 0, 0 ), Empty ) -- TODO: DRY up the ini
            , promptMode = Idle
            , submissionHistory = []
            , timeline = A.fromList []
            , uiMode = SheetEditor
            }
    in
    ( model
    , Effect.none
    )



-- UPDATE


type alias KeyCode =
    String



--| Task_This
--| NewTime Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EnterSheetEditorMode ->
            ( { model | uiMode = SheetEditor }, Effect.none )

        EnterTimelineViewerMode ->
            -- NB: we assume re-entering does not remember where you were when last viewing timeline
            --     so this is effectively JumpToLastFrame. I may want to change this, so I'm keeping
            --     it a separate Msg type
            ( { model | uiMode = TimelineViewer (A.length model.timeline) }, Effect.none )

        JumpToFirstFrame ->
            ( { model | uiMode = TimelineViewer 0 }, Effect.none )

        JumpToFrame frame ->
            ( { model | uiMode = TimelineViewer frame }, Effect.none )

        JumpToLastFrame ->
            ( { model | uiMode = TimelineViewer (A.length model.timeline) }, Effect.none )

        TogglePauseResume ->
            ( model, Effect.none )

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

                                newVal : CellElement
                                newVal =
                                    case elementAt ( newRix_, newLbl_ ) model.sheetData of
                                        Nothing ->
                                            Empty

                                        Just v ->
                                            v
                            in
                            case model.promptMode of
                                Idle ->
                                    if code == "Enter" then
                                        ( PromptInProgress "", send <| ManualDom__AttemptFocus prompt_input_dom_id, Just ( ( newRix_, newLbl_ ), newVal ) )

                                    else
                                        ( Idle, Cmd.none, Just ( ( newRix_, newLbl_ ), newVal ) )

                                PromptInProgress v ->
                                    if code == "Enter" then
                                        ( Idle, send <| PromptSubmitted ( v, ( newRix_, newLbl_ ) ), Just ( ( newRix_, newLbl_ ), newVal ) )

                                    else
                                        ( PromptInProgress v, Cmd.none, Just ( ( newRix_, newLbl_ ), newVal ) )
            in
            ( { model
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
            ( { model | keysDown = newKeys }, Effect.none )

        ClickedCell ( rix, cix ) ->
            let
                selectedValue : CellElement
                selectedValue =
                    case elementAt ( rix, cix ) model.sheetData of
                        Nothing ->
                            Empty

                        Just v ->
                            v
            in
            ( { model
                | selectedCell = Just ( ( rix, cix ), selectedValue )
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

        PromptSubmitted ( rawSub, ( rix, cix ) ) ->
            let
                newSheetCols : SheetData
                newSheetCols =
                    setValueAt ( rix, cix ) ( ( rix, cix ), str2Cell rawSub ) model.sheetData

                newHistory : List RawPrompt
                newHistory =
                    model.submissionHistory ++ [ ( rawSub, ( rix, cix ) ) ]

                newSelectedCoords =
                    ( rix + 1, cix )

                newSelectedValue =
                    case elementAt ( rix + 1, cix ) model.sheetData of
                        Nothing ->
                            Empty

                        Just v ->
                            v

                newTimeline : A.Array Timeline
                newTimeline =
                    A.append model.timeline (A.fromList [ Timeline model ])
            in
            ( { model
                | sheetData = newSheetCols
                , promptMode = Idle -- TODO: Is this redundant to key input handling?
                , submissionHistory = newHistory
                , selectedCell = Just ( newSelectedCoords, newSelectedValue )
                , timeline = newTimeline
              }
            , Effect.none
            )

        ManualDom__AttemptFocus domId ->
            ( model, Effect.fromCmd (Browser.Dom.focus domId |> Task.attempt ManualDom__FocusResult) )

        ManualDom__FocusResult result ->
            case result of
                Err _ ->
                    ( model, Effect.none )

                Ok () ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.uiMode of
        SheetEditor ->
            Sub.batch
                [ Events.onKeyDown (Decode.map KeyWentDown keyDecoder)
                , Events.onKeyUp (Decode.map KeyReleased keyDecoder)
                ]

        TimelineViewer _ ->
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


viewSheet : Model -> Element Msg
viewSheet model =
    let
        viewSheetIndex : SheetData -> Element Msg
        viewSheetIndex sheetData =
            E.table []
                { data = List.range 0 (rowCount sheetData - 1)
                , columns =
                    [ { header = E.text " "
                      , width = px 30
                      , view =
                            \rix ->
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
                                            fromInt rix
                                    )
                      }
                    ]
                }

        viewSheetColumn : ColIx -> A.Array Cell -> Element Msg
        viewSheetColumn cix column =
            let
                cellAttrs : RowIx -> List (Attribute Msg)
                cellAttrs rix =
                    let
                        shouldHighlightCell : Bool
                        shouldHighlightCell =
                            case model.selectedCell of
                                Nothing ->
                                    False

                                Just ( ( rix_, cix_ ), _ ) ->
                                    (rix_ == rix) && (cix_ == cix)

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
                    , onClick <| ClickedCell ( rix, cix )

                    --, paddingEach { top = 1, left = 0, right = 0, bottom = 1 }
                    ]

                cellContentAttrs : CellElement -> List (Attribute Msg)
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

                viewCell : Maybe ( CellCoords, CellElement ) -> String -> RowIx -> PromptMode -> Element Msg
                viewCell selectedCoords cellValueAsStr rix_ promptMode =
                    let
                        isTargetCell : Bool
                        isTargetCell =
                            case selectedCoords of
                                Nothing ->
                                    False

                                Just ( ( rix__, cix_ ), _ ) ->
                                    (rix_ == rix__) && (cix_ == cix)
                    in
                    case isTargetCell of
                        True ->
                            case promptMode of
                                Idle ->
                                    E.text cellValueAsStr

                                PromptInProgress v ->
                                    Input.text
                                        [ htmlAttribute <| HA.id prompt_input_dom_id
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
                { data = A.toList column
                , columns =
                    [ { header = E.text <| String.fromInt cix
                      , width = px 80
                      , view =
                            \( ( rix, _ ), cellElement ) ->
                                el (cellAttrs rix)
                                    (el (cellContentAttrs cellElement)
                                        (E.column (cellContentAttrs cellElement)
                                            [ viewCell model.selectedCell (cell2Str cellElement) rix model.promptMode
                                            ]
                                        )
                                    )
                      }
                    ]
                }
    in
    row [ padding 5 ] <|
        [ viewSheetIndex model.sheetData ]
            ++ (A.toList <|
                    A.map (\cix -> viewSheetColumn cix (getCol cix model.sheetData)) (A.fromList (List.range 0 (colCount model.sheetData - 1)))
               )


viewTimelinePanel : Model -> Element Msg
viewTimelinePanel model =
    case model.uiMode of
        SheetEditor ->
            Input.button
                [ Border.color UI.palette.black
                , Border.width 1
                , Border.rounded 4
                , padding 4
                , alignTop
                , Background.color UI.palette.lightGrey
                ]
                { onPress = Just <| EnterTimelineViewerMode
                , label = text "Enter Timeline Mode"
                }

        TimelineViewer frame ->
            let
                previousFrame =
                    if frame > 0 then
                        frame - 1

                    else
                        0

                nextFrame =
                    if frame == A.length model.timeline - 1 then
                        frame

                    else
                        frame + 1
            in
            E.column []
                [ E.text <|
                    "You are currently viewing frame "
                        ++ String.fromInt frame
                , E.row
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
                        { onPress = Just <| EnterSheetEditorMode
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
                        { onPress = Just <| JumpToFrame previousFrame
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
                        { onPress = Just <| JumpToFrame nextFrame
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
                ]


viewDebugPanel : Model -> Element Msg
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
            E.column [ spacing 5 ]
                [ text "Click a cell to select it, or use arrow keys to change selection. Then, press <Enter> to propose new a value for a cell, which will be submitted upon pressing <Enter> a second time"
                , text "Timeline view currently under development!"
                ]

        model_ : Model
        model_ =
            case model.uiMode of
                SheetEditor ->
                    model

                TimelineViewer i ->
                    case A.get i model.timeline of
                        Nothing ->
                            model

                        Just v ->
                            case v of
                                Timeline model__ ->
                                    { model__ | uiMode = model.uiMode }
    in
    column [ spacing 10, padding 10 ]
        [ viewInstructions
        , viewTimelinePanel model_
        , viewSheet model_
        , row
            [ spacing 5
            ]
            [ viewDebugPanel model_
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


prompt_input_dom_id : String
prompt_input_dom_id =
    -- page-scoped, static unique identifier to control focus manually
    "prompt-input-element"
