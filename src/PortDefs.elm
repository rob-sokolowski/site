port module PortDefs exposing (dragStart, elmToJS)

import Json.Decode exposing (Value)
import VegaLite exposing (Spec)



-- This is a utility module to declare port usage. I'd prefer if these we with their respective Page(s), but elm-spa
-- enforces strict module exports


port elmToJS : Spec -> Cmd msg


port dragStart : Value -> Cmd msg
