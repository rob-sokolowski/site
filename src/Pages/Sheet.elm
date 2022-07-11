module Pages.Sheet exposing (Model, Msg, page)

import Array as A
import Array2D exposing (Array2D, ColIx, RowIx, colCount, fromListOfLists, getCol, rowCount, setValueAt)
import Browser.Dom
import Browser.Events as Events
import Config exposing (apiHost)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Gen.Params.Sheet exposing (Params)
import Html.Attributes as HA
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Set exposing (Set)
import Shared
import SheetModel exposing (Cell, CellCoords, CellElement(..), RawPromptString, SheetData, array2DToSheet, elementAt)
import String exposing (fromInt)
import Task
import Time exposing (Posix)
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


type
    RenderStatus
    -- Since a lot of stuff is being rendered, initialize model to be Awaiting, and start the task of fetching
    -- browser viewport size. Once received, this will be flipped to Ready, which then renders
    -- This avoids the "double paint" problem on initial page loads (first paint assumes default screen size, followed
    -- by a correct paint)
    = AwaitingDomInfo
    | Ready


type alias Model =
    { sheetData : SheetData
    , keysDown : Set KeyCode
    , selectedCell : Maybe Cell
    , promptMode : PromptMode
    , submissionHistory : List RawPrompt
    , timeline : A.Array Timeline
    , uiMode : UiMode
    , duckDbResponse : WebData DuckDbQueryResponse
    , duckDbTableRefs : WebData DuckDbTableRefsResponse
    , userSqlText : String
    , fileUploadStatus : FileUploadStatus
    , nowish : Maybe Posix
    , viewport : Maybe Browser.Dom.Viewport
    , renderStatus : RenderStatus
    }


type FileUploadStatus
    = Idle_
    | Waiting
    | Success_
    | Fail


type alias Progress =
    Float


type alias CurrentFrame =
    Int


type UiMode
    = SheetEditor
    | TimelineViewer CurrentFrame


type Timeline
    = Timeline Model


type Msg
    = Tick Posix
    | GotViewport Browser.Dom.Viewport
    | GotResizeEvent Int Int
    | KeyWentDown KeyCode
    | KeyReleased KeyCode
    | ClickedCell CellCoords
    | PromptInputChanged String
    | PromptSubmitted RawPrompt
    | ManualDom__AttemptFocus String
    | ManualDom__FocusResult (Result Browser.Dom.Error ())
    | EnterTimelineViewerMode
    | EnterSheetEditorMode -- TODO: Just toggle UI mode?
    | QueryDuckDb String
    | UserSqlTextChanged String
      -- API response stuff:
    | GotDuckDbResponse (Result Http.Error DuckDbQueryResponse)
    | GotDuckDbTableRefsResponse (Result Http.Error DuckDbTableRefsResponse)
      -- Timeline stuff:
      -- TODO: Should Msg take in a `model` param?
    | JumpToFirstFrame
    | JumpToFrame Int
    | JumpToLastFrame
    | TogglePauseResume
      -- FileUpload Msgs
    | FileUpload_UserClickedSelectFile
    | FileUpload_UserSelectedCsvFile File
    | FileUpload_UploadResponded (Result Http.Error ())



--| FileUpload_GotProgress Http.Progress


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
        initSqlText =
            """select
    p.sponsor_ids
from president_polls_historical p
limit 5
"""

        data : Array2D CellElement
        data =
            let
                col =
                    List.repeat 25 Empty

                rows =
                    List.repeat 100 col
            in
            fromListOfLists rows

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
            , duckDbResponse = NotAsked
            , userSqlText = initSqlText
            , fileUploadStatus = Idle_
            , nowish = Nothing
            , viewport = Nothing
            , duckDbTableRefs = NotAsked
            , renderStatus = AwaitingDomInfo
            }
    in
    ( model
    , Effect.fromCmd <|
        Cmd.batch
            [ Task.perform GotViewport Browser.Dom.getViewport
            , fetchDuckDbTableRefs
            ]
    )



-- UPDATE


type alias KeyCode =
    String



--| Task_This
--| NewTime Time.Posix


mapColumnsToSheet : List Column -> SheetData
mapColumnsToSheet cols =
    let
        mapVal : Val -> CellElement
        mapVal v =
            case v of
                Varchar_ var ->
                    String_ var

                Int__ i ->
                    Int_ i

                Unknown ->
                    Empty

        -- lol is "list of lists", but I'm also laughing at how inefficient this is
        -- TODO: I think it'd be worthwhile to refactor Array2D to accept column lists not row-lists
        lolWrong =
            List.map (\col -> List.map (\e -> mapVal e) col.vals) cols

        lolTransposed =
            LE.transpose lolWrong
    in
    array2DToSheet <| fromListOfLists lolTransposed


uploadFile : Model -> File -> Cmd Msg
uploadFile model f =
    let
        nowish_ =
            case model.nowish of
                Nothing ->
                    -- HACK: as long as `Tick` is implemented at 250 ms chances of this occurring is very low
                    --       good enough
                    Time.posixToMillis (Time.millisToPosix 99999999)

                Just n ->
                    Time.posixToMillis n
    in
    Http.request
        { method = "POST"
        , url = apiHost ++ "/duckdb/files"
        , headers = []
        , body =
            Http.multipartBody
                [ Http.filePart "file" f
                , Http.stringPart "duckdb_table_ref" ("elm_test_" ++ String.fromInt nowish_)
                ]
        , expect = Http.expectWhatever FileUpload_UploadResponded
        , timeout = Nothing
        , tracker = Just "upload"
        }


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick now ->
            ( { model | nowish = Just now }, Effect.none )

        GotResizeEvent _ _ ->
            -- rather than keeping two copies of this info in memory, chain a resize event
            -- to the existing flow on first page render. This should avoid strange resizing
            -- frames from being rendered.. at least I hope so!
            ( { model | renderStatus = AwaitingDomInfo }, Effect.fromCmd (Task.perform GotViewport Browser.Dom.getViewport) )

        GotViewport viewport ->
            ( { model | viewport = Just viewport, renderStatus = Ready }, Effect.none )

        GotDuckDbTableRefsResponse response ->
            case response of
                Ok refs ->
                    ( { model | duckDbTableRefs = Success refs }, Effect.none )

                Err err ->
                    ( { model | duckDbTableRefs = Failure err }, Effect.none )

        FileUpload_UserClickedSelectFile ->
            ( model, Effect.fromCmd requestFile )

        FileUpload_UserSelectedCsvFile csv ->
            ( model
            , Effect.fromCmd <| uploadFile model csv
            )

        FileUpload_UploadResponded result ->
            ( model, Effect.fromCmd fetchDuckDbTableRefs )

        UserSqlTextChanged newText ->
            ( { model | userSqlText = newText }, Effect.none )

        QueryDuckDb queryStr ->
            ( { model | duckDbResponse = Loading }, Effect.fromCmd <| queryDuckDb queryStr )

        GotDuckDbResponse response ->
            case response of
                Ok data ->
                    --let
                    --    convertToSheet : DuckDbQueryResponse -> SheetData
                    --    convertToSheet data_ =
                    --        array2DToSheet <| fromListOfLists (List.map (\e -> [ String_ e ]) data_.columns)
                    --in
                    ( { model
                        | duckDbResponse = Success data
                        , sheetData = mapColumnsToSheet data.columns
                      }
                    , Effect.none
                    )

                Err err ->
                    ( { model | duckDbResponse = Failure err }, Effect.none )

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
    let
        contextualKeystrokes =
            case model.uiMode of
                SheetEditor ->
                    Sub.batch
                        [ Events.onKeyDown (JD.map KeyWentDown keyDecoder)
                        , Events.onKeyUp (JD.map KeyReleased keyDecoder)
                        ]

                TimelineViewer _ ->
                    Sub.none
    in
    Sub.batch
        [ contextualKeystrokes
        , Time.every 500 Tick
        , Events.onResize GotResizeEvent
        ]


keyDecoder : JD.Decoder String
keyDecoder =
    JD.field "key" JD.string



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            "Sheet Demo"

        elements : Model -> Element Msg
        elements mdl =
            case mdl.renderStatus of
                AwaitingDomInfo ->
                    E.none

                Ready ->
                    E.column
                        [ E.width E.fill
                        , E.height E.fill
                        , Background.color UI.palette.white
                        , Font.size 12
                        , padding 5
                        ]
                        [ content mdl
                        ]

        content : Model -> Element Msg
        content mdl =
            let
                viewInstructions : Element Msg
                viewInstructions =
                    E.column [ spacing 5 ]
                        [ text "Click a cell to select it, or use arrow keys to change selection. Then, press <Enter> to propose new a value for a cell, which will be submitted upon pressing <Enter> a second time"
                        , text "This app is under development, there are bugs, but there shouldn't be any crashes"
                        ]

                model_ : Model
                model_ =
                    case mdl.uiMode of
                        SheetEditor ->
                            mdl

                        TimelineViewer i ->
                            case A.get i mdl.timeline of
                                Nothing ->
                                    mdl

                                Just v ->
                                    case v of
                                        Timeline model__ ->
                                            { model__ | uiMode = mdl.uiMode }

                ( w, h ) =
                    case mdl.viewport of
                        Nothing ->
                            ( 800, 600 )

                        Just viewport ->
                            ( round viewport.viewport.width - 20, round viewport.viewport.height - 20 )
            in
            el
                [ width (E.fill |> maximum w)
                , height (E.fill |> maximum h)
                , Border.width 1
                , Border.color UI.palette.black
                , padding 5
                , spacing 5
                ]
                (row
                    [ width (E.fill |> maximum w)
                    , height (E.fill |> maximum h)
                    , spacing 5
                    ]
                    [ el
                        [ width <| E.fillPortion 8
                        , height <| E.fill
                        , Border.width 2
                        , Border.color UI.palette.blue
                        , clip
                        , scrollbars
                        ]
                        (viewSheet model)
                    , el
                        [ width <| E.fillPortion 2
                        , height E.fill
                        ]
                        (column
                            [ height E.fill
                            , width E.fill

                            --, padding 5
                            , Border.width 1
                            , Border.color UI.palette.lightGrey
                            , spacing 5
                            ]
                            [ el
                                [ width E.fill
                                , height <| E.fillPortion 4
                                , Border.width 1
                                , Border.color UI.palette.lightGrey
                                ]
                                (viewCatalogPanel model)
                            , el
                                [ width E.fill
                                , height <| E.fillPortion 4
                                , Border.width 1
                                , Border.color UI.palette.lightGrey
                                ]
                                (viewSqlInputPanel model)
                            , el
                                [ width E.fill
                                , height <| E.fillPortion 2
                                , Border.width 1
                                , Border.color UI.palette.lightGrey
                                ]
                                (viewDebugPanel model)
                            ]
                        )
                    ]
                )

        --
        --
        --column [ spacing 10, padding 10 ]
        --    [ viewInstructions
        --    , viewSqlInputPanel model_
        --    , viewTimelinePanel model_
        --    , viewUploadFile model_
        --    , viewSheet model_
        --    , row
        --        [ spacing 5
        --        ]
        --        [ viewDebugPanel model_
        --        ]
        --    ]
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


viewSqlInputPanel : Model -> Element Msg
viewSqlInputPanel model =
    let
        viewDuckDbButton : Element Msg
        viewDuckDbButton =
            Input.button
                [ Border.color UI.palette.black
                , Border.width 1
                , Border.rounded 4
                , padding 4
                , alignTop
                , alignRight
                , Background.color UI.palette.lightGrey
                ]
                { onPress = Just <| QueryDuckDb model.userSqlText
                , label = text "Query DuckDB"
                }

        viewSqlInput : Element Msg
        viewSqlInput =
            Input.multiline
                [ width <| maximum 450 fill
                , height <| px 150
                , Border.rounded 6
                , Border.width 2
                , Border.color <| rgb255 0x72 0x9F 0xCF
                ]
                { onChange = UserSqlTextChanged
                , text = model.userSqlText
                , placeholder = Just <| Input.placeholder [] <| text "Type your message"
                , label = Input.labelAbove [] <| text "Enter a sql query:"
                , spellcheck = True
                }

        viewError : Element Msg
        viewError =
            let
                errAttrs =
                    el
                        [ Background.color UI.palette.lightGrey
                        , Border.width 2
                        , Border.color UI.palette.darkishGrey
                        ]
            in
            case model.duckDbResponse of
                Failure err ->
                    case err of
                        BadUrl url ->
                            errAttrs <| text <| "Bad url: " ++ url

                        Timeout ->
                            errAttrs <| text <| "Request timed out!"

                        BadStatus int ->
                            errAttrs <| text <| "Http status: " ++ String.fromInt int

                        NetworkError ->
                            errAttrs <| text <| "An unknown network error!"

                        BadBody s ->
                            errAttrs <| text <| "Bad body: " ++ s

                _ ->
                    E.none
    in
    E.column []
        [ viewSqlInput
        , viewDuckDbButton
        , viewError
        ]


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
                            text <| "    " ++ rawStr ++ " @:(" ++ String.fromInt rix ++ ", " ++ String.fromInt lbl ++ ")"
            in
            column []
                [ text <| "Submission history:"
                , column [ spacing 2 ] <| List.map promptStr history
                ]
    in
    column
        [ width E.fill
        , height E.fill
        , spacing 2
        ]
        [ text "Debug info:"
        , text keyString
        , text selectedCoordsStr
        , text selectedValueStr
        , viewPromptHistory model.submissionHistory
        ]


requestFile : Cmd Msg
requestFile =
    Select.file [ "application/csv" ] FileUpload_UserSelectedCsvFile


viewCatalogPanel : Model -> Element Msg
viewCatalogPanel model =
    let
        viewTableRefs : Model -> Element Msg
        viewTableRefs mdl =
            let
                s =
                    case mdl.duckDbTableRefs of
                        NotAsked ->
                            text "Didn't request data yet"

                        Loading ->
                            text "Fetching..."

                        Success refsResponse ->
                            column
                                [ spacing 1
                                ]
                                ([ text "DuckDB Refs:" ]
                                    ++ List.map (\ref -> text <| "  " ++ ref) refsResponse.refs
                                )

                        Failure err ->
                            text "Error"
            in
            s

        viewUploadFile : Model -> Element Msg
        viewUploadFile mdl =
            Input.button
                [ alignBottom
                , alignRight
                , padding 5
                , Border.color UI.palette.black
                , Border.width 1
                , Border.rounded 3
                , Background.color UI.palette.lightGrey
                ]
                { onPress = Just FileUpload_UserClickedSelectFile
                , label = text "Upload CSV File"
                }
    in
    column
        [ width E.fill
        , height E.fill
        , clip
        ]
        [ viewTableRefs model
        , viewUploadFile model
        ]



-- API
-- utils


send : Msg -> Cmd Msg
send m =
    Task.succeed m
        |> Task.perform identity


prompt_input_dom_id : String
prompt_input_dom_id =
    -- page-scoped, static unique identifier to control focus manually
    "prompt-input-element"



-- API - TODO: I'd like for these to be in it's own tested module


fetchDuckDbTableRefs : Cmd Msg
fetchDuckDbTableRefs =
    let
        duckDbTableRefsResponseDecoder : JD.Decoder DuckDbTableRefsResponse
        duckDbTableRefsResponseDecoder =
            JD.map DuckDbTableRefsResponse
                (JD.field "refs" (JD.list JD.string))
    in
    Http.get
        { url = apiHost ++ "/duckdb/table_refs"
        , expect = Http.expectJson GotDuckDbTableRefsResponse duckDbTableRefsResponseDecoder
        }


queryDuckDb : String -> Cmd Msg
queryDuckDb query =
    let
        duckDbQueryEncoder : String -> JE.Value
        duckDbQueryEncoder q =
            JE.object
                [ ( "query_str", JE.string q )
                , ( "allow_blob_fallback", JE.bool False )
                , ( "fallback_table_refs", JE.list JE.string [] )
                ]

        duckDbQueryResponseDecoder : JD.Decoder DuckDbQueryResponse
        duckDbQueryResponseDecoder =
            let
                columnDecoderHelper : JD.Decoder Column
                columnDecoderHelper =
                    JD.field "type" JD.string |> JD.andThen decoderByType

                decoderByType : String -> JD.Decoder Column
                decoderByType type_ =
                    case type_ of
                        "VARCHAR" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.map Varchar_ JD.string)))

                        "INTEGER" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.map Int__ JD.int)))

                        _ ->
                            -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                            -- Should this fail loudly?
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.list (JD.succeed Unknown))
            in
            JD.map DuckDbQueryResponse
                (JD.field "columns" (JD.list columnDecoderHelper))
    in
    Http.post
        { url = apiHost ++ "/duckdb"
        , body = Http.jsonBody (duckDbQueryEncoder query)
        , expect = Http.expectJson GotDuckDbResponse duckDbQueryResponseDecoder
        }


type alias Column =
    { name : String
    , type_ : String
    , vals : List Val
    }


type Val
    = Varchar_ String
      --| Bool_ Bool
      --| Float_ Float
    | Int__ Int
    | Unknown


type alias TypeRef =
    String


type alias DuckDbQueryResponse =
    { columns : List Column
    }


type alias DuckDbTableRefsResponse =
    { refs : List String
    }
