module Gen.Params.Snippets.InactionIsAnAction exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "snippets" </> Parser.s "inaction-is-an-action")

