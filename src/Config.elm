module Config exposing (apiHost)


env =
    --Local
    Production



--Production


type Env
    = Local
    | Production


apiHost =
    case env of
        Local ->
            "http://localhost:8000"

        Production ->
            "https://api.robsoko.tech"
