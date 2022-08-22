module Utils exposing (..)

import Json.Decode as JD
import Regex
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


collapseWhitespace : String -> String
collapseWhitespace s =
    -- collapses all whitespace chains to a single " "
    -- ex: "  input  \t \n   string" -> " input string"
    case Regex.fromString "\\s+" of
        Nothing ->
            s

        Just rex ->
            Regex.replace rex (\_ -> " ") s
