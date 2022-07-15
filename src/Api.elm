module Api exposing (..)


type alias Column =
    { name : String
    , type_ : String
    , vals : List (Maybe Val)
    }


type Val
    = Varchar_ String
    | Bool__ Bool
    | Float__ Float
    | Int__ Int
    | Unknown


type alias TableRef =
    String


type alias DuckDbQueryResponse =
    { columns : List Column
    }


type alias DuckDbTableRefsResponse =
    { refs : List TableRef
    }
