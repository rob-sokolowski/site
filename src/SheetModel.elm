module SheetModel exposing (..)

import Array as A
import Array.Extra as AE
import Array2D exposing (Array2D, ColIx, RowIx)
import ISO8601 as Iso



-- this module provides a sheet-specific wrapper around an Array2D


type CellElement
    = Empty
    | String_ String
    | Time_ Iso.Time
    | Float_ Float
    | Int_ Int
    | Bool_ Bool


type alias RawPromptString =
    String


type alias CellCoords =
    ( RowIx, ColIx )


type alias Cell =
    ( CellCoords, CellElement )


type alias ColumnLabel =
    String


type alias SheetEnvelope =
    { data : Array2D Cell
    , columnLabels : List ColumnLabel
    }


elementAt : ( RowIx, ColIx ) -> SheetEnvelope -> Maybe CellElement
elementAt ( rix, cix ) sheet =
    case Array2D.getValueAt ( rix, cix ) sheet.data of
        -- throw away coords
        Just ( _, e ) ->
            Just e

        Nothing ->
            Nothing


array2DToSheet : Array2D CellElement -> List ColumnLabel -> SheetEnvelope
array2DToSheet arr2d colLabels =
    -- given an Array2D of elements, convert to sheet data
    -- this is useful for initialization
    let
        nRows =
            Array2D.rowCount arr2d

        nCols =
            Array2D.colCount arr2d
    in
    { data =
        AE.map2
            (\row rix ->
                AE.map2 (\e cix -> ( ( rix, cix ), e ))
                    row
                    (A.fromList <| List.range 0 (nCols - 1))
            )
            arr2d
            (A.fromList <| List.range 0 (nRows - 1))
    , columnLabels = colLabels
    }
