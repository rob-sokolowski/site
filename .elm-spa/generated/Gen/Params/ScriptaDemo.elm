module Gen.Params.ScriptaDemo exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "scripta-demo")

