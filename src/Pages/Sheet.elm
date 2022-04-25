module Pages.Sheet exposing (Model, Msg, page)

import Array
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
    }


type alias RowNumber =
    Int


type alias Index =
    Array.Array TableIndex


type TableIndex
    = Origin
    | RowIdx RowNumber


type alias ColumnData =
    { label : String
    , col : List CellData
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
        Origin ->
            " "

        RowIdx ix ->
            String.fromInt ix


type alias RowData =
    { index_ : TableIndex
    , a : CellData
    , b : CellData
    , c : CellData
    , d : CellData
    , e : CellData
    , f : CellData
    , g : CellData
    , h : CellData
    , i : CellData
    , j : CellData
    , k : CellData
    , l : CellData
    , m : CellData
    , n : CellData
    , o : CellData
    , p : CellData
    , q : CellData
    , r : CellData
    , s : CellData
    , t : CellData
    , u : CellData
    , v : CellData
    , w : CellData
    , x : CellData
    , y : CellData
    , z : CellData
    }


init : ( Model, Effect Msg )
init =
    let
        rowCount =
            10

        rowIx =
            Array.initialize rowCount identity

        tableIndex =
            Array.append (Array.fromList [ Origin ]) (Array.map (\e -> RowIdx e) rowIx)

        columnCount =
            7

        labels =
            Array.fromList [ "A", "B", "C", "D", "E", "F", "G" ]

        columns =
            Array.map (\lbl -> ColumnData lbl (List.map (\e -> Empty) (Array.toList rowIx))) labels
    in
    ( { sheetIdx = tableIndex
      , sheetColumns = columns
      , sheetRowCount = rowCount
      }
    , Effect.none
    )



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
                                    , paddingEach { top = 1, bottom = 1, left = 0, right = 0 }
                                    ]
                                    (el [ centerX ] <| E.text <| index2Str r)
                      }
                    ]
                }

        viewSheetColumns : ColumnData -> Element Msg
        viewSheetColumns column =
            let
                cellAttrs : CellData -> List (Attribute msg)
                cellAttrs cd =
                    [ Border.color UI.palette.lightGrey
                    , Border.width 1

                    --, paddingEach { top = 1, left = 0, right = 0, bottom = 1 }
                    ]

                cellContentAttrs : CellData -> List (Attribute msg)
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
                            \r -> el (cellAttrs r) (el (cellContentAttrs r) (el (cellContentAttrs r) (E.text (cell2Str r))))
                      }
                    ]
                }
    in
    row [ padding 5 ] <|
        [ viewSheetIndex model.sheetIdx ]
            ++ (Array.toList <|
                    Array.map (\e -> viewSheetColumns e) model.sheetColumns
               )


content : Model -> Element Msg
content model =
    column []
        [ sheet model
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
