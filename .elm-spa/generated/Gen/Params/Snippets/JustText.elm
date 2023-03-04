module Gen.Params.Snippets.JustText exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "snippets" </> Parser.s "just-text")

