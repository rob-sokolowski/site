module Gen.Params.ParableOfPolygonsClone exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "parable-of-polygons-clone")

