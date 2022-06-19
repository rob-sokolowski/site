module DuckDbApi exposing (..)

import Config exposing (apiHost)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


type Msg
    = GotDuckReponse (Result Http.Error DuckDbQueryResponse)


queryDuckDb : String -> Cmd Msg
queryDuckDb query =
    Debug.todo "implement decoder"



--Http.post
--    { url = apiHost ++ "/duckdb"
--    , body = Http.jsonBody (duckDbQueryEncoder query)
--    , expect = Http.expectJson GotDuckReponse duckDbQueryResponseDecoder
--    }


duckDbQueryEncoder : String -> JE.Value
duckDbQueryEncoder query =
    JE.object
        [ ( "query_str", JE.string query )
        ]


mainDecoder : JD.Decoder Column
mainDecoder =
    let
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
                        (JD.field "values" (JD.list (JD.map Int_ JD.int)))

                _ ->
                    -- This feels wrong to me, but unsure how else to workaround the string pattern matching
                    JD.map3 Column
                        (JD.field "name" JD.string)
                        (JD.field "type" JD.string)
                        (JD.list (JD.succeed Unknown))
    in
    JD.field "type" JD.string |> JD.andThen decoderByType


type alias Column =
    { name : String
    , type_ : String
    , vals : List Val
    }


type Val
    = Varchar_ String
      --| Bool_ Bool
      --| Float_ Float
    | Int_ Int
    | Unknown


type alias DuckDbQueryResponse =
    { columns : List Column
    }
