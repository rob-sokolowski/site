module Utils exposing (..)

import Json.Decode as JD
import Task


removeNothingsFromList : List (Maybe a) -> List a
removeNothingsFromList list =
    List.filterMap identity list


keyDecoder : JD.Decoder String
keyDecoder =
    JD.field "key" JD.string


send : msg -> Cmd msg
send m =
    Task.succeed m
        |> Task.perform identity
