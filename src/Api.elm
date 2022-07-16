module Api exposing (..)


type alias Column =
    { name : String
    , type_ : String
    , vals : List (Maybe Val)
    }


type alias ColumnDescription =
    { name : String
    , type_ : String
    }


type Val
    = Varchar_ String
    | Bool_ Bool
    | Float_ Float
    | Int_ Int
    | Unknown


type alias TableRef =
    String


type alias DuckDbQueryResponse =
    { columns : List Column
    }


type alias DuckDbMetaResponse =
    { colDescs : List ColumnDescription
    }


type alias DuckDbTableRefsResponse =
    { refs : List TableRef
    }
