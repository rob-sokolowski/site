port module VegaPort exposing (elmToJS)

import VegaLite exposing (Spec)


port elmToJS : Spec -> Cmd msg
