port module VegaPort exposing (elmToJS, myVis)

import Platform
import VegaLite exposing (..)


myVis : Spec
myVis =
    let
        data =
            dataFromColumns []
                << dataColumn "x" (nums [ 10, 20, 30 ])

        enc =
            encoding
                << position X [ pName "x", pQuant ]
    in
    toVegaLite
        [ title "Hello, World!" []
        , data []
        , enc []
        , circle []
        ]


port elmToJS : Spec -> Cmd msg
