module Geo.Coordinates exposing
    ( GeoPoint
    , Vec3
    , angularDistance
    , clamp
    , cross
    , dot
    , fromUnitVector
    , latitudeLongitudeToUnitVector
    , normalize
    , scale
    , subtract
    )


{-| Geographic coordinates are degrees. The globe coordinate convention is
`+Y` north, `+X` at 0° longitude, and `+Z` at 90° east longitude.
-}
type alias GeoPoint =
    { latitude : Float
    , longitude : Float
    }


type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


latitudeLongitudeToUnitVector : GeoPoint -> Vec3
latitudeLongitudeToUnitVector point =
    let
        latitude = degrees point.latitude
        longitude = degrees point.longitude
    in
    { x = cos latitude * cos longitude
    , y = sin latitude
    , z = cos latitude * sin longitude
    }


fromUnitVector : Vec3 -> GeoPoint
fromUnitVector vector =
    let
        unit = normalize vector
    in
    { latitude = atan2 unit.y (sqrt (unit.x * unit.x + unit.z * unit.z)) |> radiansToDegrees
    , longitude = atan2 unit.z unit.x |> radiansToDegrees
    }


angularDistance : GeoPoint -> GeoPoint -> Float
angularDistance first second =
    let
        a = latitudeLongitudeToUnitVector first
        b = latitudeLongitudeToUnitVector second
    in
    acos (clamp -1 1 (dot a b))


dot : Vec3 -> Vec3 -> Float
dot first second =
    first.x * second.x + first.y * second.y + first.z * second.z


cross : Vec3 -> Vec3 -> Vec3
cross first second =
    { x = first.y * second.z - first.z * second.y
    , y = first.z * second.x - first.x * second.z
    , z = first.x * second.y - first.y * second.x
    }


subtract : Vec3 -> Vec3 -> Vec3
subtract first second =
    { x = first.x - second.x, y = first.y - second.y, z = first.z - second.z }


scale : Float -> Vec3 -> Vec3
scale amount vector =
    { x = amount * vector.x, y = amount * vector.y, z = amount * vector.z }


normalize : Vec3 -> Vec3
normalize vector =
    let
        magnitude = sqrt (dot vector vector)
    in
    if magnitude == 0 then
        { x = 0, y = 0, z = 0 }

    else
        scale (1 / magnitude) vector


clamp : Float -> Float -> Float -> Float
clamp low high value =
    min high (max low value)


radiansToDegrees : Float -> Float
radiansToDegrees angle =
    angle * 180 / pi
