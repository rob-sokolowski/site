module SheetModel exposing (..)

import Array as A
import Array.Extra as AE
import Array2D exposing (Array2D, ColIx, RowIx)


type CellElement
    = Empty
    | String_ String
    | Float_ Float
    | Int_ Int
    | Bool_ Bool


type alias RawPromptString =
    String


type alias CellCoords =
    ( RowIx, ColIx )


type alias Cell =
    ( CellCoords, CellElement )


type alias SheetData =
    Array2D Cell


elementAt : ( RowIx, ColIx ) -> SheetData -> Maybe CellElement
elementAt ( rix, cix ) sheet =
    let
        cell : Maybe Cell
        cell =
            Array2D.getValueAt ( rix, cix ) sheet
    in
    case cell of
        -- throw away coords
        Just ( _, e ) ->
            Just e

        Nothing ->
            Nothing


array2DToSheet : Array2D CellElement -> SheetData
array2DToSheet arr2d =
    -- given an Array2D of elements, convert to sheet data
    -- this is useful for initialization
    let
        nRows =
            Array2D.rowCount arr2d

        nCols =
            Array2D.colCount arr2d
    in
    AE.map2
        (\row rix ->
            AE.map2 (\e cix -> ( ( rix, cix ), e ))
                row
                (A.fromList <| List.range 0 (nCols - 1))
        )
        arr2d
        (A.fromList <| List.range 0 (nRows - 1))
