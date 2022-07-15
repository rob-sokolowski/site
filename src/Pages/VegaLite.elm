module Pages.VegaLite exposing (Model, Msg, page)

import Api as Api exposing (DuckDbQueryResponse, TableRef, Val(..))
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
    | RenderPlot VL.Spec
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
                    "select rank, spi from elm_test_1657819905432"
            in
            ( model, Effect.fromCmd <| queryDuckDbForPlot queryStr False [] )

        RenderPlot spec ->
            ( model, Effect.fromCmd <| elmToJS spec )



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
            { onPress = Just <| RenderPlot myVis
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


viewQueryBuilder : Element Msg
viewQueryBuilder =
    let
        vegaLiteDiv =
            el
                [ htmlAttribute <| HA.id "elm-ui-viz"
                , Border.color UI.palette.black
                , Border.width 2

                --, width <| px 10
                --, height <| px 10
                ]
                E.none
    in
    column
        [ --htmlAttribute <| HA.id "elm-ui-viz"
          Border.width 2
        , Border.color UI.palette.black
        , padding 10
        , spacing 10

        --, width <| px 200
        --, height <| px 400
        --, alignTop
        ]
        [ -- HACK: in order to 'send' our vega spec to elmToJs, we must trigger a Cmd Msg
          Input.button
            [ Border.color UI.palette.black
            , Border.width 1
            , Border.rounded 4
            , padding 4
            , alignTop
            , alignRight
            , Background.color UI.palette.lightGrey
            ]
            { onPress = Just <| RenderPlot myVis
            , label = text "Render Plot"
            }
        , vegaLiteDiv
        ]


path : String
path =
    "https://cdn.jsdelivr.net/npm/vega-datasets@2.2/data/"


myVis : VL.Spec
myVis =
    let
        weatherColors =
            VL.categoricalDomainMap
                [ ( "sun", "#e7ba52" )
                , ( "fog", "#c7c7c7" )
                , ( "drizzle", "#aec7ea" )
                , ( "rain", "#1f77b4" )
                , ( "snow", "#9467bd" )
                ]

        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName "temp_max", VL.pBin [] ]
                << VL.position VL.Y [ VL.pAggregate VL.opCount ]
                << VL.color [ VL.mName "weather", VL.mScale weatherColors ]
    in
    VL.toVegaLite
        [ VL.dataFromUrl (path ++ "seattle-weather.csv") []
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
