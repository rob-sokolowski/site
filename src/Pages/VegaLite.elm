module Pages.VegaLite exposing (Model, Msg, page)

import Api
import Array
import Config exposing (apiHost)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.VegaLite exposing (Params)
import Html as H
import Html.Attributes as HA
import Html5.DragDrop as DragDrop
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import Page
import Palette
import PortDefs exposing (dragStart, elmToJS)
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Shared
import Utils exposing (removeNothingsFromList)
import VegaLite as VL
import VegaUtils exposing (ColumnParamed, mapColToFloatCol, mapColToIntegerCol)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Position
    = Up
    | Middle
    | Down



-- INIT


type alias Model =
    { spec : Maybe VL.Spec
    , duckDbForPlotResponse : WebData Api.DuckDbQueryResponse
    , duckDbMetaResponse : WebData Api.DuckDbMetaResponse
    , duckDbTableRefs : WebData Api.DuckDbTableRefsResponse
    , selectedTableRef : Maybe Api.TableRef
    , hoveredOnTableRef : Maybe Api.TableRef
    , dragDrop : DragDrop.Model Int Position
    , data : { count : Int, position : Position }
    }


type alias DragId =
    String


type alias DropId =
    String


init : ( Model, Effect Msg )
init =
    ( { spec = Nothing
      , duckDbForPlotResponse = NotAsked
      , duckDbMetaResponse = NotAsked
      , duckDbTableRefs = Loading -- Must also fetch table refs below
      , selectedTableRef = Nothing
      , hoveredOnTableRef = Nothing
      , dragDrop = DragDrop.init
      , data = { count = 1, position = Middle }
      }
    , Effect.fromCmd fetchDuckDbTableRefs
    )



-- UPDATE
--


type Msg
    = FetchPlotData
    | RenderPlot
    | FetchTableRefs
    | FetchMetaDataForRef Api.TableRef
    | GotDuckDbForPlotResponse (Result Http.Error Api.DuckDbQueryResponse)
    | GotDuckDbMetaResponse (Result Http.Error Api.DuckDbMetaResponse)
    | GotDuckDbTableRefsResponse (Result Http.Error Api.DuckDbTableRefsResponse)
    | UserSelectedTableRef Api.TableRef
    | UserMouseEnteredTableRef Api.TableRef
    | UserMouseLeftTableRef
    | DragDropMsg (DragDrop.Msg Int Position)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model
                | dragDrop = model_
                , data =
                    case result of
                        Nothing ->
                            model.data

                        Just ( count, position, _ ) ->
                            { count = count + 1, position = position }
              }
            , Effect.fromCmd
                (DragDrop.getDragstartEvent msg_
                    |> Maybe.map (.event >> dragStart)
                    |> Maybe.withDefault Cmd.none
                )
            )

        FetchTableRefs ->
            ( { model | duckDbTableRefs = Loading }, Effect.fromCmd <| fetchDuckDbTableRefs )

        GotDuckDbTableRefsResponse response ->
            case response of
                Ok refs ->
                    ( { model | duckDbTableRefs = Success refs }, Effect.none )

                Err err ->
                    ( { model | duckDbTableRefs = Failure err }, Effect.none )

        FetchMetaDataForRef ref ->
            let
                -- NB: A bit hacky, but we submit a query with limit 0, and use the same response without vals
                queryStr =
                    "select * from " ++ ref ++ " limit 0"
            in
            ( { model | duckDbTableRefs = Loading }, Effect.fromCmd <| queryDuckDbMeta queryStr True [ ref ] )

        GotDuckDbMetaResponse response ->
            case response of
                Ok data ->
                    ( { model
                        | duckDbMetaResponse = Success data
                      }
                    , Effect.none
                    )

                Err err ->
                    ( { model | duckDbMetaResponse = Failure err }, Effect.none )

        FetchPlotData ->
            let
                queryStr =
                    """select
  t.rank,
  t.spi
from elm_test_1657972702341 t
order by 1
limit 100
                """
            in
            ( model, Effect.fromCmd <| queryDuckDbForPlot queryStr True [ "elm_test_1657972702341" ] )

        GotDuckDbForPlotResponse response ->
            case response of
                Ok data ->
                    ( { model
                        | duckDbForPlotResponse = Success data
                      }
                    , Effect.none
                    )

                Err err ->
                    ( { model | duckDbForPlotResponse = Failure err }, Effect.none )

        RenderPlot ->
            let
                newSpec =
                    computeSpec model

                elmToJsCmd =
                    case newSpec of
                        Nothing ->
                            Cmd.none

                        Just spec ->
                            elmToJS spec
            in
            ( { model | spec = newSpec }, Effect.fromCmd elmToJsCmd )

        UserSelectedTableRef ref ->
            let
                -- NB: A bit hacky, but we submit a query with limit 0, and use the same response without vals
                queryStr =
                    "select * from " ++ ref ++ " limit 0"
            in
            ( { model | duckDbMetaResponse = Loading, selectedTableRef = Just ref }
            , Effect.fromCmd <| queryDuckDbMeta queryStr True [ ref ]
            )

        UserMouseEnteredTableRef ref ->
            ( { model | hoveredOnTableRef = Just ref }, Effect.none )

        UserMouseLeftTableRef ->
            ( { model | hoveredOnTableRef = Nothing }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- begin region view


view : Model -> View Msg
view model =
    let
        title =
            "VegaLite Demo"
    in
    { title = title
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            , Font.size 12
            , padding 5
            ]
            (elements model)
        ]
    }


elements : Model -> Element Msg
elements model =
    let
        vegaLiteDiv =
            el
                [ htmlAttribute <| HA.id "elm-ui-viz"
                , Border.color Palette.lightGrey
                , Border.width 1
                , width fill
                , height fill
                ]
                E.none
    in
    row
        [ Border.width 1
        , Border.color Palette.red
        , width fill
        , height fill
        ]
        [ column
            [ height fill
            , width <| fillPortion 8
            , padding 5
            , clipX
            , scrollbarX
            ]
            [ el
                [ width <| fill
                , height <| fillPortion 4
                , clipY
                , scrollbarY
                , Border.width 1
                , Border.color Palette.darkishGrey
                ]
                (viewColumnPickerPanel model)
            , row
                [ height <| fillPortion 6
                , width fill
                , clipY
                , scrollbarY
                ]
                [ el
                    [ width <| fillPortion 5
                    , height fill
                    , Border.width 1
                    , Border.color Palette.darkishGrey
                    ]
                    (el [ height fill, width <| fillPortion 5 ] (E.text "droppable zone"))

                --(viewPlotPanel model)
                , el [ height fill, width <| fillPortion 5 ] (E.text "QB output zone")
                ]
            ]
        , el
            [ height fill
            , width <| fillPortion 2
            , Border.width 1
            , Border.color Palette.darkishGrey
            , padding 5
            ]
            (viewTableRefs model)
        ]


type KimballColumn
    = Dimension
    | Measure Aggregation
    | Time
    | Error


type Aggregation
    = Sum
    | Mean
    | Median
    | Min
    | Max


mapToKimball : Api.ColumnDescription -> KimballColumn
mapToKimball colDesc =
    case colDesc.type_ of
        "VARCHAR" ->
            Dimension

        "DATE" ->
            Time

        "BOOLEAN" ->
            Dimension

        "INTEGER" ->
            Measure Sum

        "DOUBLE" ->
            Measure Sum

        _ ->
            Error


viewColumnPickerPanel : Model -> Element Msg
viewColumnPickerPanel model =
    case model.duckDbMetaResponse of
        NotAsked ->
            el [] (text "Select a table to plot from the right nav")

        Loading ->
            el [] (text "Loading")

        Success data ->
            let
                dimCols : List Api.ColumnDescription -> List Api.ColumnDescription
                dimCols cols =
                    List.filter (\c -> mapToKimball c == Dimension) cols

                timeCols : List Api.ColumnDescription -> List Api.ColumnDescription
                timeCols cols =
                    List.filter (\c -> mapToKimball c == Time) cols

                measureCols : List Api.ColumnDescription -> List Api.ColumnDescription
                measureCols cols =
                    List.filter
                        (\c ->
                            -- accept all sub-variants of measures
                            List.member (mapToKimball c)
                                [ Measure Sum
                                , Measure Mean
                                , Measure Median
                                , Measure Min
                                , Measure Max
                                ]
                        )
                        cols

                errorCols : List Api.ColumnDescription -> List Api.ColumnDescription
                errorCols cols =
                    List.filter (\c -> mapToKimball c == Error) cols

                viewColDescTab : Api.ColumnDescription -> E.Color -> Element Msg
                viewColDescTab colDesc color =
                    column
                        [ Border.width 1
                        , Border.color Palette.darkishGrey
                        , spacing 15
                        , padding 5
                        , Background.color color
                        ]
                        [ text colDesc.name
                        , text colDesc.type_
                        ]
            in
            row
                [ spacing 10
                ]
                [ column
                    [ alignTop
                    , width fill
                    , Border.width 1
                    , Border.color Palette.black
                    ]
                    [ text "Dimensions:"
                    , wrappedRow [] <| List.map (\col -> viewColDescTab col Palette.blue_light) (dimCols data.colDescs)
                    ]
                , column
                    [ alignTop
                    , width fill
                    , Border.width 1
                    , Border.color Palette.black
                    ]
                    [ text "Time:"
                    , wrappedRow [] <| List.map (\col -> viewColDescTab col Palette.yellow_mustard) (timeCols data.colDescs)
                    ]
                , column
                    [ alignTop
                    , width fill
                    , Border.width 1
                    , Border.color Palette.black
                    ]
                    [ text "Measures:"
                    , wrappedRow [] <| List.map (\col -> viewColDescTab col Palette.green_keylime) (measureCols data.colDescs)
                    ]
                , column
                    [ alignTop
                    , width fill
                    , Border.width 1
                    , Border.color Palette.black
                    ]
                    [ text "Errors:"
                    , wrappedRow [] <| List.map (\col -> viewColDescTab col Palette.orange_error_alert) (errorCols data.colDescs)
                    ]
                ]

        Failure err ->
            el [] (text "Error!")


viewPlotPanel : Model -> Element Msg
viewPlotPanel model =
    let
        viewDragDropElements : Model -> Element Msg
        viewDragDropElements model_ =
            column
                [ width fill
                , height fill
                ]
                [ viewDiv Up model_.data
                , viewDiv Middle model_.data
                , viewDiv Down model_.data
                ]

        viewDiv : Position -> { count : Int, position : Position } -> Element Msg
        viewDiv position data =
            let
                droppableAttrs : List (Attribute Msg)
                droppableAttrs =
                    if data.position /= position then
                        List.map E.htmlAttribute (DragDrop.droppable DragDropMsg position)

                    else
                        []
            in
            el
                ([ width fill
                 , height fill
                 , Border.color Palette.darkCharcoal
                 , Border.width 5
                 ]
                    ++ droppableAttrs
                )
                (if data.position == position then
                    column
                        [ width fill
                        , height fill
                        ]
                        [ image
                            ([ width (px 200)
                             , height (px 200)
                             , centerX
                             , centerY
                             ]
                                ++ List.map E.htmlAttribute (DragDrop.draggable DragDropMsg data.count)
                            )
                            { src = "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
                            , description = "Elm logo (placeholder)"
                            }
                        , el [ centerX, centerY ] <| E.text (String.fromInt data.count)
                        ]

                 else
                    E.none
                )
    in
    el
        [ width (px 800)
        , height (px 800)
        , Border.color Palette.red
        , Border.width 1
        ]
        (viewDragDropElements model)


viewTableRefs : Model -> Element Msg
viewTableRefs model =
    case model.duckDbTableRefs of
        NotAsked ->
            text "Didn't request data yet"

        Loading ->
            text "Fetching..."

        Success refsResponse ->
            let
                refsSelector : List Api.TableRef -> Element Msg
                refsSelector refs =
                    let
                        backgroundColorFor ref =
                            case model.hoveredOnTableRef of
                                Nothing ->
                                    Palette.white

                                Just ref_ ->
                                    if ref == ref_ then
                                        Palette.lightGrey

                                    else
                                        Palette.white

                        borderColorFor ref =
                            case model.hoveredOnTableRef of
                                Nothing ->
                                    Palette.white

                                Just ref_ ->
                                    if ref == ref_ then
                                        Palette.darkishGrey

                                    else
                                        Palette.white

                        borderFor ref =
                            case model.hoveredOnTableRef of
                                Nothing ->
                                    { top = 1, left = 0, right = 0, bottom = 1 }

                                Just ref_ ->
                                    if ref == ref_ then
                                        { top = 1, left = 0, right = 0, bottom = 1 }

                                    else
                                        { top = 1, left = 0, right = 0, bottom = 1 }

                        innerBlobColorFor ref =
                            case model.hoveredOnTableRef of
                                Nothing ->
                                    Palette.white

                                Just ref_ ->
                                    if ref == ref_ then
                                        Palette.black

                                    else
                                        Palette.white

                        ui : Api.TableRef -> Element Msg
                        ui ref =
                            row
                                [ width E.fill
                                , paddingXY 0 2
                                , spacingXY 2 0
                                , onClick <| UserSelectedTableRef ref
                                , onMouseEnter <| UserMouseEnteredTableRef ref
                                , onMouseLeave <| UserMouseLeftTableRef
                                , Background.color (backgroundColorFor ref)
                                , Border.widthEach (borderFor ref)
                                , Border.color (borderColorFor ref)
                                ]
                                [ el
                                    [ width <| px 5
                                    , height <| px 5
                                    , Border.width 1
                                    , Background.color (innerBlobColorFor ref)
                                    ]
                                    E.none
                                , text ref
                                ]
                    in
                    column
                        [ width E.fill
                        , height E.fill
                        , paddingXY 5 0
                        ]
                    <|
                        List.map (\ref -> ui ref) refs
            in
            column
                [ width E.fill
                , height E.fill
                , spacing 2
                ]
                [ text "DuckDB Refs:"
                , refsSelector refsResponse.refs
                ]

        Failure err ->
            text "Error"



-- end region view
-- begin region vega-lite


computeSpec : Model -> Maybe VL.Spec
computeSpec model =
    let
        spec0 : ColumnParamed Int -> ColumnParamed Float -> VL.Spec
        spec0 col1 col2 =
            let
                data =
                    VL.dataFromColumns []
                        << VL.dataColumn col1.name (VL.nums (List.map (\i -> toFloat i) col1.vals))
                        << VL.dataColumn col2.name (VL.nums col2.vals)

                enc =
                    VL.encoding
                        << VL.position VL.X [ VL.pName col1.name, VL.pQuant ]
                        << VL.position VL.Y [ VL.pName col2.name, VL.pQuant ]
            in
            VL.toVegaLite
                [ data []
                , VL.line []
                , enc []
                , VL.height 400
                , VL.width 600
                ]
    in
    case model.duckDbForPlotResponse of
        NotAsked ->
            Nothing

        Loading ->
            Nothing

        Failure err ->
            Nothing

        Success data ->
            let
                collArray =
                    Array.fromList data.columns

                col1 =
                    case Array.get 0 collArray of
                        Nothing ->
                            { name = "error"
                            , vals = []
                            }

                        Just col ->
                            mapColToIntegerCol col

                col2 =
                    case Array.get 1 collArray of
                        Nothing ->
                            { name = "error"
                            , vals = []
                            }

                        Just col ->
                            mapColToFloatCol col
            in
            Just (spec0 col1 col2)



-- end region vega-lite
-- begin region query building
-- end region query building
-- begin region API


queryDuckDbForPlot : String -> Bool -> List Api.TableRef -> Cmd Msg
queryDuckDbForPlot query allowFallback refs =
    let
        duckDbQueryEncoder : JE.Value
        duckDbQueryEncoder =
            JE.object
                [ ( "query_str", JE.string query )
                , ( "allow_blob_fallback", JE.bool allowFallback )
                , ( "fallback_table_refs", JE.list JE.string refs )
                ]

        duckDbQueryResponseDecoder : JD.Decoder Api.DuckDbQueryResponse
        duckDbQueryResponseDecoder =
            let
                columnDecoderHelper : JD.Decoder Api.Column
                columnDecoderHelper =
                    JD.field "type" JD.string |> JD.andThen decoderByType

                decoderByType : String -> JD.Decoder Api.Column
                decoderByType type_ =
                    case type_ of
                        "VARCHAR" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Api.Varchar_ JD.string))))

                        "INTEGER" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Api.Int_ JD.int))))

                        "BOOLEAN" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Api.Bool_ JD.bool))))

                        "DOUBLE" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Api.Float_ JD.float))))

                        "DATE" ->
                            -- TODO: Need to think about Elm date / time types
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Api.Varchar_ JD.string))))

                        _ ->
                            -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                            -- Should this fail loudly?
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.list (JD.maybe (JD.succeed Api.Unknown)))
            in
            JD.map Api.DuckDbQueryResponse
                (JD.field "columns" (JD.list columnDecoderHelper))
    in
    Http.post
        { url = apiHost ++ "/duckdb"
        , body = Http.jsonBody duckDbQueryEncoder
        , expect = Http.expectJson GotDuckDbForPlotResponse duckDbQueryResponseDecoder
        }


queryDuckDbMeta : String -> Bool -> List Api.TableRef -> Cmd Msg
queryDuckDbMeta query allowFallback refs =
    let
        duckDbQueryEncoder : JE.Value
        duckDbQueryEncoder =
            JE.object
                [ ( "query_str", JE.string query )
                , ( "allow_blob_fallback", JE.bool allowFallback )
                , ( "fallback_table_refs", JE.list JE.string refs )
                ]

        duckDbMetaResponseDecoder : JD.Decoder Api.DuckDbMetaResponse
        duckDbMetaResponseDecoder =
            let
                columnDecoderHelper : JD.Decoder Api.ColumnDescription
                columnDecoderHelper =
                    JD.field "type" JD.string |> JD.andThen decoderByType

                decoderByType : String -> JD.Decoder Api.ColumnDescription
                decoderByType type_ =
                    case type_ of
                        "VARCHAR" ->
                            JD.map2 Api.ColumnDescription
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)

                        "INTEGER" ->
                            JD.map2 Api.ColumnDescription
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)

                        _ ->
                            -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                            -- Should this fail loudly?
                            JD.map2 Api.ColumnDescription
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
            in
            JD.map Api.DuckDbMetaResponse
                (JD.field "columns" (JD.list columnDecoderHelper))
    in
    Http.post
        { url = apiHost ++ "/duckdb"
        , body = Http.jsonBody duckDbQueryEncoder
        , expect = Http.expectJson GotDuckDbMetaResponse duckDbMetaResponseDecoder
        }


fetchDuckDbTableRefs : Cmd Msg
fetchDuckDbTableRefs =
    let
        duckDbTableRefsResponseDecoder : JD.Decoder Api.DuckDbTableRefsResponse
        duckDbTableRefsResponseDecoder =
            JD.map Api.DuckDbTableRefsResponse
                (JD.field "refs" (JD.list JD.string))
    in
    Http.get
        { url = apiHost ++ "/duckdb/table_refs"
        , expect = Http.expectJson GotDuckDbTableRefsResponse duckDbTableRefsResponseDecoder
        }



-- end region API
