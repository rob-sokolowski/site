module Gen.Params.SmartSqueeze exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "smart-squeeze")

