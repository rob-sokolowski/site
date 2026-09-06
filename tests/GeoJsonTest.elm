module GeoJsonTest exposing (suite)

import Expect
import Geography.GeoJson as GeoJson
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "GeoJSON country-border conversion"
        [ test "decodes polygon coordinates as longitude then latitude" <|
            \_ ->
                Decode.decodeString GeoJson.featureCollectionDecoder polygonFixture
                    |> Result.map (List.concatMap GeoJson.borderSegments)
                    |> Result.map List.length
                    |> Expect.equal (Ok 3)
        , test "an antimeridian edge remains short after conversion to 3D" <|
            \_ ->
                let
                    segments =
                        GeoJson.borderSegments
                            (GeoJson.LineString
                                [ { latitude = 0, longitude = 179.9 }
                                , { latitude = 0, longitude = -179.9 }
                                ]
                            )
                in
                case List.head segments of
                    Just ( first, second ) ->
                        sqrt ((first.x - second.x) ^ 2 + (first.y - second.y) ^ 2 + (first.z - second.z) ^ 2)
                            |> Expect.lessThan 0.01

                    Nothing ->
                        Expect.fail "Expected an antimeridian segment"
        ]


polygonFixture : String
polygonFixture =
    "{\"features\":[{\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[10,20],[11,20],[11,21],[10,20]]]}}]}"
