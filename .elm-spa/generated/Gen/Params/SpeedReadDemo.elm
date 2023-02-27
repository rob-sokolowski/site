module Gen.Params.SpeedReadDemo exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "speed-read-demo")

