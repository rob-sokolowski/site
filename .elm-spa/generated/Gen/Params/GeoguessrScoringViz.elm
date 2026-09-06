module Gen.Params.GeoguessrScoringViz exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "geoguessr-scoring-viz")

