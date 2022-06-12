module Array2D exposing (ColIx, RowIx, fromListOfLists, getCol, getRow, getValueAt, setValueAt)

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
    -- This function feels inefficient..
    let
        nRows : Int
        nRows =
            A.length arr2d

        colWithMaybes : List (Maybe e)
        colWithMaybes =
            List.map2 (\rix cix_ -> getValueAt ( rix, cix_ ) arr2d) (L.range 0 (nRows - 1)) (L.repeat nRows cix)
    in
    A.fromList <| removeNothingFromList colWithMaybes
