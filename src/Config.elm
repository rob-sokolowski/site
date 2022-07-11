module Config exposing (apiHost)


env =
    LocalDev



--| Production
--Production


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
            "https://api.robsoko.tech"
