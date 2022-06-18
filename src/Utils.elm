module Utils exposing (..)


removeNothingFromList : List (Maybe a) -> List a
removeNothingFromList list =
    List.filterMap identity list
