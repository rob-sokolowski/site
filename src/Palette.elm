module Palette exposing (..)

import Color
import Element as E exposing (..)
import Element.Font as Font


globalLayoutAttrs : List (Attribute msg)
globalLayoutAttrs =
    [ padding 2
    , Font.family [ Font.typeface "Open Sans", Font.sansSerif ]
    , Font.size 16
    , width fill
    ]


topLevelBlogAttrs : List (Attribute msg)
topLevelBlogAttrs =
    [ width (fill |> maximum 900 |> minimum 250)
    , height fill
    , centerX
    ]


blue =
    rgb255 0x72 0x9F 0xCF


darkCharcoal =
    rgb255 0x2E 0x34 0x36


lightBlue =
    rgb255 0xC5 0xE8 0xF7


lightGrey =
    rgb255 0xE3 0xE3 0xE6


white =
    rgb255 0xFF 0xFF 0xFF


black =
    rgb255 0x00 0x00 0x00


darkishGrey =
    rgb255 0xAB 0xAA 0xB2


red =
    rgb255 0xFF 0x12 0x10



-- THEME - inspired by Elm logo


blue_light : Color
blue_light =
    rgb255 0x60 0xB5 0xCC


blue_darker =
    rgb255 0x5A 0x63 0x78


yellow_mustard =
    rgb255 0xF0 0xAD 0x00


yellow_wordle =
    rgb255 0xCC 0xC1 0x33


green_keylime =
    rgb255 0x7F 0xD1 0x3B


orange_error_alert =
    rgb255 0xFC 0x8F 0x32


toAvhColor : Color -> Color.Color
toAvhColor color =
    -- I typically work with Element.Color in UIs, but Svg gets along better with Avh's Color,
    -- so to keep the palette defined in one place, transform between the two
    let
        rgba =
            toRgb color
    in
    Color.rgba rgba.red rgba.green rgba.blue rgba.alpha
