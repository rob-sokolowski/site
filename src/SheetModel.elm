module SheetModel exposing (..)


type CellData
    = Empty
    | String_ String
    | Float_ Float
    | Int_ Int
    | Bool_ Bool


type alias RawPromptString =
    String
