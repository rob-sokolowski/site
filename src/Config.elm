module Config exposing (apiHost)


env =
    Production



--LocalDev


type Env
    = LocalDev
    | LocalGunicorn
    | Production


apiHost =
    case env of
        LocalDev ->
            "http://localhost:8000"

        LocalGunicorn ->
            "http://localhost:8080"

        Production ->
            "https://fir-api.robsoko.tech"
