module Geo.Contours exposing (contour, scoreLevels)

import Geo.Coordinates exposing (GeoPoint, Vec3, cross, latitudeLongitudeToUnitVector, normalize, scale)
import Geo.GeoGuessrScore exposing (ScoreModel, distanceForScore)


{-| Generate a small circle directly on the unit sphere. It uses an orthonormal
basis around the target vector, rather than latitude/longitude interpolation.
-}
contour : ScoreModel -> GeoPoint -> Float -> Int -> List Vec3
contour model target requestedScore sampleCount =
    let
        center = latitudeLongitudeToUnitVector target
        angle = distanceForScore model requestedScore / model.earthRadiusKm
        reference =
            if abs center.y < 0.9 then
                { x = 0, y = 1, z = 0 }

            else
                { x = 1, y = 0, z = 0 }

        east = normalize (cross reference center)
        north = normalize (cross center east)
        sample index =
            let
                phi = 2 * pi * toFloat index / toFloat sampleCount
                ring = add (scale (cos phi) east) (scale (sin phi) north)
            in
            add (scale (cos angle) center) (scale (sin angle) ring)
    in
    List.range 0 (sampleCount - 1) |> List.map sample


scoreLevels : Int -> List Float
scoreLevels interval =
    let
        next level =
            if level < interval then
                []

            else
                toFloat level :: next (level - interval)
    in
    next (5000 - interval)


add : Vec3 -> Vec3 -> Vec3
add first second =
    { x = first.x + second.x, y = first.y + second.y, z = first.z + second.z }
