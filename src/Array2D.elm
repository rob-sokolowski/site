module Array2D exposing (Array2D, ColIx, RowIx, colCount, fromListOfLists, getCol, getRow, getValueAt, rowCount, setValueAt)

-- Implements basic 2D array structure. I didn't consider performance at all, and all 2D arrays are assumed
-- to be regular. Where "regular" means all rows have the same length and all cols have the same length
-- (but isn't necessarily a square matrix)

import Array as A
import List as L
import Utils exposing (removeNothingFromList)


type alias Array2D e =
    A.Array (A.Array e)


type alias RowIx =
    Int


type alias ColIx =
    Int


fromListOfLists : List (List e) -> Array2D e
fromListOfLists lol =
    A.fromList <|
        List.map (\l -> A.fromList l) lol


getValueAt : ( RowIx, ColIx ) -> Array2D e -> Maybe e
getValueAt ( rix, cix ) arr2d =
    case A.get rix arr2d of
        Just row ->
            case A.get cix row of
                Just v ->
                    Just v

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


setValueAt : ( RowIx, ColIx ) -> e -> Array2D e -> Array2D e
setValueAt ( rix, cix ) val arr2d =
    case A.get rix arr2d of
        Just row ->
            let
                newRow =
                    A.set cix val row
            in
            A.set rix newRow arr2d

        Nothing ->
            arr2d


getRow : RowIx -> Array2D e -> A.Array e
getRow rix arr2d =
    case A.get rix arr2d of
        Just row ->
            row

        Nothing ->
            A.empty


getCol : ColIx -> Array2D e -> A.Array e
getCol cix arr2d =
    -- This function feels inefficient.. Also, weird, silent stuff might happen if the array isn't "regular"
    let
        nRows : Int
        nRows =
            A.length arr2d

        colWithMaybes : List (Maybe e)
        colWithMaybes =
            List.map (\rix -> getValueAt ( rix, cix ) arr2d) (L.range 0 (nRows - 1))
    in
    A.fromList <| removeNothingFromList colWithMaybes


rowCount : Array2D e -> Int
rowCount arr2d =
    A.length arr2d


colCount : Array2D e -> Int
colCount arr2d =
    -- Assumes square array2d!
    A.length (getRow 0 arr2d)
