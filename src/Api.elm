module Api exposing (..)

import Config exposing (apiHost)
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import RemoteData exposing (RemoteData, asCmd)


type alias Column =
    { name : String
    , type_ : String
    , vals : List (Maybe Val)
    }


type alias ColumnDescription =
    { name : String
    , type_ : String
    }


type Val
    = Varchar_ String
    | Bool_ Bool
    | Float_ Float
    | Int_ Int
    | Unknown


type alias TableRef =
    String


type alias DuckDbQueryResponse =
    { columns : List Column
    }


type alias DuckDbMetaResponse =
    { colDescs : List ColumnDescription
    }


type alias DuckDbTableRefsResponse =
    { refs : List TableRef
    }


queryDuckDb : String -> Bool -> List TableRef -> (Result Error DuckDbQueryResponse -> msg) -> Cmd msg
queryDuckDb query allowFallback refs onResponse =
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
                                (JD.field "values" (JD.list (JD.maybe (JD.map Varchar_ JD.string))))

                        "INTEGER" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Int_ JD.int))))

                        "BIGINT" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Int_ JD.int))))

                        "BOOLEAN" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Bool_ JD.bool))))

                        "DOUBLE" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Float_ JD.float))))

                        "DATE" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Varchar_ JD.string))))

                        "TIMESTAMP" ->
                            JD.map3 Column
                                (JD.field "name" JD.string)
                                (JD.field "type" JD.string)
                                (JD.field "values" (JD.list (JD.maybe (JD.map Varchar_ JD.string))))

                        _ ->
                            -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                            -- Should this fail loudly?
                            JD.map3 Column
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
        , expect = Http.expectJson onResponse duckDbQueryResponseDecoder
        }
