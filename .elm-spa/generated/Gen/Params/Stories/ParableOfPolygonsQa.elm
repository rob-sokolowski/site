module Gen.Params.Stories.ParableOfPolygonsQa exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "stories" </> Parser.s "parable-of-polygons-qa")

