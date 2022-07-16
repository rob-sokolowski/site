module Pages.VegaLite exposing (Model, Msg, page)

import Api as Api exposing (DuckDbQueryResponse, TableRef, Val(..), mapColToFloatCol, mapColToIntegerCol)
import Array
import Config exposing (apiHost)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.VegaLite exposing (Params)
import Html.Attributes as HA
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Shared
import UI
import Utils exposing (removeNothingFromList)
import VegaLite as VL
import VegaPort exposing (elmToJS)
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
    { spec : Maybe VL.Spec
    , duckDbForPlotResponse : WebData DuckDbQueryResponse
    }


init : ( Model, Effect Msg )
init =
    ( { spec = Nothing
      , duckDbForPlotResponse = NotAsked
      }
    , Effect.none
    )



-- UPDATE
--


type Msg
    = FetchPlotData
    | RenderPlot
    | GotDuckDbForPlotResponse (Result Http.Error DuckDbQueryResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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
            ( model, Effect.fromCmd <| queryDuckDbForPlot queryStr False [] )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


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
            , padding 10
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
                , Border.color UI.palette.lightGrey
                , Border.width 1
                , width fill
                , height fill
                ]
                E.none
    in
    column
        [ --htmlAttribute <| HA.id "elm-ui-viz"
          Border.width 2
        , Border.color UI.palette.black

        --, width <| px 600
        --, height <| px 400
        --, alignLeft
        , padding 10
        , spacing 10

        --, width <| px 200
        --, height <| px 400
        --, alignTop
        ]
        [ Input.button
            [ Border.color UI.palette.black
            , Border.width 1
            , Border.rounded 4
            , padding 4
            , Background.color UI.palette.lightGrey
            ]
            { onPress = Just RenderPlot
            , label = text "Render Plot"
            }
        , Input.button
            [ Border.color UI.palette.black
            , Border.width 1
            , Border.rounded 4
            , padding 4
            , Background.color UI.palette.lightGrey
            ]
            { onPress = Just <| FetchPlotData
            , label = text "Fetch Plot Data"
            }
        , vegaLiteDiv
        ]


computeSpec : Model -> Maybe VL.Spec
computeSpec model =
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


spec0 : Api.Column2 Int -> Api.Column2 Float -> VL.Spec
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


queryDuckDbForPlot : String -> Bool -> List TableRef -> Cmd Msg
queryDuckDbForPlot query allowFallback refs =
    let
        duckDbQueryEncoder : JE.Value
        duckDbQueryEncoder =
            JE.object
                [ ( "query_str", JE.string query )
                , ( "allow_blob_fallback", JE.bool allowFallback )
                , ( "fallback_table_refs", JE.list JE.string refs )
                ]

        duckDbQueryResponseDecoder : JD.Decoder DuckDbQueryResponse
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
                                (JD.field "values" (JD.list (JD.maybe (JD.map Varchar_ JD.string))))

                        "INTEGER" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Int__ JD.int))))

                        "BOOLEAN" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Bool__ JD.bool))))

                        "DOUBLE" ->
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Float__ JD.float))))

                        "DATE" ->
                            -- TODO: Need to think about Elm date / time types
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Varchar_ JD.string))))

                        _ ->
                            -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                            -- Should this fail loudly?
                            JD.map3 Api.Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.list (JD.maybe (JD.succeed Unknown)))
            in
            JD.map DuckDbQueryResponse
                (JD.field "columns" (JD.list columnDecoderHelper))
    in
    Http.post
        { url = apiHost ++ "/duckdb"
        , body = Http.jsonBody duckDbQueryEncoder
        , expect = Http.expectJson GotDuckDbForPlotResponse duckDbQueryResponseDecoder
        }
