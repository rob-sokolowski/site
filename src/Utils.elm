module Utils exposing (..)

import Json.Decode as JD


removeNothingsFromList : List (Maybe a) -> List a
removeNothingsFromList list =
    List.filterMap identity list


keyDecoder : JD.Decoder String
keyDecoder =
    JD.field "key" JD.string
