module PromptParser exposing (parseCellData)

import Parser exposing ((|.), (|=), Parser, chompWhile, float, getChompedString, int, number, oneOf, spaces, succeed, symbol)
import SheetModel exposing (CellElement(..), RawPromptString)


parseCellData : RawPromptString -> CellElement
parseCellData str =
    let
        parser_ : Parser CellElement
        parser_ =
            oneOf
                [ succeed Int_
                    |. spaces
                    |= int
                    |. spaces
                , succeed Float_
                    |. spaces
                    |= float
                    |. spaces

                --, succeed Empty -- Empty must be before String_, since "" is also a valid String
                --, succeed String_
                --    |. spaces
                --    |= (getChompedString <| chompWhile Char.isAlphaNum)
                --    |. spaces
                ]
    in
    case Parser.run parser_ str of
        Ok cellData ->
            cellData

        Err _ ->
            Empty
