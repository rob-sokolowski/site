module Gen.Params.ElmUiSvgIssue exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    ()


parser =
    (Parser.s "elm-ui-svg-issue")

