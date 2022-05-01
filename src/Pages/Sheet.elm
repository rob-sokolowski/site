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
import Shared
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


type alias Model =
    { sheetIdx : Index
    , sheetColumns : Array.Array ColumnData
    , sheetRowCount : Int
    , keyDown : Maybe KeyCode
    , selectedCoords : Maybe ( RowIx, ColumnLabel )
    , selectedValue : Maybe CellData
    }


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
            Array.map (\lbl -> ColumnData lbl (List.map (\rix -> ( rix, String_ "Yo!" )) (Array.toList rowIx))) labels
    in
    ( { sheetIdx = tableIndex
      , sheetColumns = columns
      , sheetRowCount = rowCount
      , keyDown = Nothing
      , selectedCoords = Nothing
      , selectedValue = Nothing
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


type Msg
    = KeyDowns KeyCode
    | ClearPressed
    | ClickedCell ( RowIx, ColumnLabel )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        KeyDowns code ->
            ( { model | keyDown = Just code }, Effect.none )

        ClearPressed ->
            ( { model | keyDown = Nothing }, Effect.none )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDowns keyDecoder)
        , Events.onKeyUp (Decode.succeed ClearPressed)
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
                    [ Border.color UI.palette.lightGrey
                    , Border.width 1
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
            in
            E.table
                [ padding 0 ]
                { data = column.col
                , columns =
                    [ { header = E.text column.label
                      , width = px 80
                      , view =
                            \( rix, cellValue ) -> el (cellAttrs rix cellValue) (el (cellContentAttrs cellValue) (el (cellContentAttrs cellValue) (E.text (cell2Str cellValue))))
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
        keyString =
            case model.keyDown of
                Nothing ->
                    "No keys down"

                Just key ->
                    key

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
    in
    column
        [ padding 5
        , Border.color UI.palette.black
        , Border.width 2
        ]
        [ text keyString
        , text selectedCoordsStr
        , text selectedValueStr
        ]


content : Model -> Element Msg
content model =
    column []
        [ sheet model
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
        [ --header
          content model

        --, footer
        ]
