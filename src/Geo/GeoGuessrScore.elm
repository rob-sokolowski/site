module Geo.GeoGuessrScore exposing
    ( ScoreModel
    , distance
    , distanceForScore
    , earthRadiusKm
    , score
    , worldModel
    )

import Geo.Coordinates exposing (GeoPoint, angularDistance)


{-| `mapScaleKm` is deliberately a model input: GeoGuessr scoring varies by map.
The initial 20,000 km value is the conventional World-map scale approximation.
-}
type alias ScoreModel =
    { mapScaleKm : Float
    , earthRadiusKm : Float
    }


earthRadiusKm : Float
earthRadiusKm =
    6371.0088


worldModel : ScoreModel
worldModel =
    { mapScaleKm = 20000
    , earthRadiusKm = earthRadiusKm
    }


distance : ScoreModel -> GeoPoint -> GeoPoint -> Float
distance model first second =
    model.earthRadiusKm * angularDistance first second


score : ScoreModel -> GeoPoint -> GeoPoint -> Float
score model target candidate =
    min 5000 (5000 * (e ^ (-10 * distance model target candidate / model.mapScaleKm)))


distanceForScore : ScoreModel -> Float -> Float
distanceForScore model requestedScore =
    if requestedScore <= 0 then
        1 / 0

    else if requestedScore >= 5000 then
        0

    else
        -(model.mapScaleKm / 10) * logBase e (requestedScore / 5000)
