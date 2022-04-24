module Pages.Sheet exposing (Model, Msg, page)

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
    { sheetData : List RowData
    }


type alias RowNumber =
    Int


type TableIndex
    = Origin
    | RowIdx RowNumber


type CellData
    = Empty_
    | String_ String
    | Float_ Float
    | Int_ Int
    | Bool_ Bool


cell2Str : CellData -> String
cell2Str cd =
    case cd of
        Empty_ ->
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
        emptyRow : RowNumber -> RowData
        emptyRow rn =
            { index_ = RowIdx rn
            , a = Int_ 1
            , b = Float_ 3.14
            , c = Bool_ True
            , d = String_ "ABC!"
            , e = Empty_
            , f = Empty_
            , g = Empty_
            , h = Empty_
            , i = Empty_
            , j = Empty_
            , k = Empty_
            , l = Empty_
            , m = Empty_
            , n = Empty_
            , o = Empty_
            , p = Empty_
            , q = Empty_
            , r = Empty_
            , s = Empty_
            , t = Empty_
            , u = Empty_
            , v = Empty_
            , w = Empty_
            , x = Empty_
            , y = Empty_
            , z = Empty_
            }
    in
    ( { sheetData = [ emptyRow 1, emptyRow 2, emptyRow 3, emptyRow 4, emptyRow 5, emptyRow 6 ]
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
    { title = "Pops"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (elements model)
        ]
    }


table : Model -> Element Msg
table model =
    let
        cellAttrs : CellData -> List (Attribute msg)
        cellAttrs cd =
            [ Border.color UI.palette.lightGrey
            , Border.width 1
            , paddingEach { top = 1, left = 0, right = 0, bottom = 1 }
            ]

        cellContentAttrs : CellData -> List (Attribute msg)
        cellContentAttrs cd =
            let
                alignment =
                    case cd of
                        Empty_ ->
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
            ]
    in
    E.table
        [ padding 5 ]
        { data = model.sheetData
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
                            (el [ centerX ] <| E.text <| index2Str r.index_)
              }
            , { header = E.text "A"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.a)
                            (el (cellContentAttrs r.a) <| el (cellContentAttrs r.a) <| E.text <| cell2Str r.a)
              }
            , { header = E.text "B"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.b)
                            (el (cellContentAttrs r.b) <| E.text <| cell2Str r.b)
              }
            , { header = E.text "C"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.c)
                            (el (cellContentAttrs r.c) <| E.text <| cell2Str r.c)
              }
            , { header = E.text "D"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.d)
                            (el (cellContentAttrs r.d) <| E.text <| cell2Str r.d)
              }
            , { header = E.text "E"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.e)
                            (el (cellContentAttrs r.e) <| E.text <| cell2Str r.e)
              }
            , { header = E.text "F"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.f)
                            (el (cellContentAttrs r.f) <| E.text <| cell2Str r.f)
              }
            , { header = E.text "G"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.g)
                            (el (cellContentAttrs r.g) <| E.text <| cell2Str r.g)
              }
            , { header = E.text "H"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.h)
                            (el (cellContentAttrs r.h) <| E.text <| cell2Str r.h)
              }
            , { header = E.text "I"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.i)
                            (el (cellContentAttrs r.i) <| E.text <| cell2Str r.i)
              }
            , { header = E.text "J"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.j)
                            (el (cellContentAttrs r.j) <| E.text <| cell2Str r.j)
              }
            , { header = E.text "K"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.k)
                            (el (cellContentAttrs r.k) <| E.text <| cell2Str r.k)
              }
            , { header = E.text "L"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.l)
                            (el (cellContentAttrs r.l) <| E.text <| cell2Str r.l)
              }
            , { header = E.text "M"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.m)
                            (el (cellContentAttrs r.m) <| E.text <| cell2Str r.m)
              }
            , { header = E.text "N"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.n)
                            (el (cellContentAttrs r.n) <| E.text <| cell2Str r.n)
              }
            , { header = E.text "O"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.o)
                            (el (cellContentAttrs r.o) <| E.text <| cell2Str r.o)
              }
            , { header = E.text "P"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.p)
                            (el (cellContentAttrs r.p) <| E.text <| cell2Str r.p)
              }
            , { header = E.text "Q"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.q)
                            (el (cellContentAttrs r.q) <| E.text <| cell2Str r.q)
              }
            , { header = E.text "R"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.r)
                            (el (cellContentAttrs r.r) <| E.text <| cell2Str r.r)
              }
            , { header = E.text "S"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.s)
                            (el (cellContentAttrs r.s) <| E.text <| cell2Str r.s)
              }
            , { header = E.text "T"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.t)
                            (el (cellContentAttrs r.t) <| E.text <| cell2Str r.t)
              }
            , { header = E.text "U"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.u)
                            (el (cellContentAttrs r.u) <| E.text <| cell2Str r.u)
              }
            , { header = E.text "V"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.v)
                            (el (cellContentAttrs r.v) <| E.text <| cell2Str r.v)
              }
            , { header = E.text "W"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.w)
                            (el (cellContentAttrs r.w) <| E.text <| cell2Str r.w)
              }
            , { header = E.text "X"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.x)
                            (el (cellContentAttrs r.x) <| E.text <| cell2Str r.x)
              }
            , { header = E.text "Y"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.y)
                            (el (cellContentAttrs r.y) <| E.text <| cell2Str r.y)
              }
            , { header = E.text "Z"
              , width = px 200
              , view =
                    \r ->
                        el
                            (cellAttrs r.z)
                            (el (cellContentAttrs r.z) <| E.text <| cell2Str r.z)
              }
            ]
        }


content : Model -> Element Msg
content model =
    column []
        [ table model
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
