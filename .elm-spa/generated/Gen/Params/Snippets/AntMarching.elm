module Gen.Params.Snippets.AntMarching exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "snippets" </> Parser.s "ant-marching")

