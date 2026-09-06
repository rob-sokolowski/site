module GeoTest exposing (suite)

import Expect
import Geo.Contours as Contours
import Geo.Coordinates as Coordinates exposing (GeoPoint)
import Geo.GeoGuessrScore as Score
import Test exposing (Test, describe, test)


closeTo : Float -> Float -> Expect.Expectation
closeTo expected actual =
    Expect.within (Expect.Absolute 0.0001) expected actual


suite : Test
suite =
    describe "geographic score mathematics"
        [ test "equator quarter circumference is about 10,008 km" <|
            \_ ->
                Score.distance Score.worldModel zero zeroEast
                    |> Expect.within (Expect.Absolute 1) 10007.56
        , test "coordinate conversion round-trips a non-axis-aligned location" <|
            \_ ->
                let
                    original = { latitude = -33.8688, longitude = 151.2093 }
                    roundTrip = original |> Coordinates.latitudeLongitudeToUnitVector |> Coordinates.fromUnitVector
                in
                Expect.all
                    [ \_ -> closeTo original.latitude roundTrip.latitude
                    , \_ -> closeTo original.longitude roundTrip.longitude
                    ]
                    ()
        , test "score inversion recreates the requested score" <|
            \_ ->
                let
                    requested = 3200
                    distance = Score.distanceForScore Score.worldModel requested
                    recreated = 5000 * (e ^ (-10 * distance / Score.worldModel.mapScaleKm))
                in
                closeTo requested recreated
        , test "every sampled contour point has its requested score" <|
            \_ ->
                let
                    target = { latitude = 40.7128, longitude = -74.006 }
                    expectedScore = 3500
                    points = Contours.contour Score.worldModel target expectedScore 96
                    scores =
                        points
                            |> List.map (Coordinates.fromUnitVector >> Score.score Score.worldModel target)
                in
                Expect.all
                    (scores
                        |> List.map (\actual -> \_ -> Expect.within (Expect.Absolute 0.001) expectedScore actual)
                    )
                    ()
        , test "contour levels honor the selected interval and exclude degenerate 5000 circle" <|
            \_ ->
                let
                    levels = Contours.scoreLevels 100
                in
                Expect.all
                    [ \_ -> Expect.equal [ 4900, 4800, 4700 ] (List.take 3 levels)
                    , \_ -> Expect.equal [ 200, 100 ] (List.reverse levels |> List.take 2 |> List.reverse)
                    ]
                    ()
        ]


zero : GeoPoint
zero =
    { latitude = 0, longitude = 0 }


zeroEast : GeoPoint
zeroEast =
    { latitude = 0, longitude = 90 }
