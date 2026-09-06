module Geography.GeoJson exposing (Geometry(..), borderSegments, featureCollectionDecoder)

import Geo.Coordinates exposing (GeoPoint, Vec3, latitudeLongitudeToUnitVector, scale)
import Json.Decode as Decode


type Geometry
    = Polygon (List (List GeoPoint))
    | MultiPolygon (List (List (List GeoPoint)))
    | LineString (List GeoPoint)


featureCollectionDecoder : Decode.Decoder (List Geometry)
featureCollectionDecoder =
    Decode.field "features" (Decode.list featureDecoder)
        |> Decode.map (List.filterMap identity)


featureDecoder : Decode.Decoder (Maybe Geometry)
featureDecoder =
    Decode.field "geometry" (Decode.maybe geometryDecoder)


geometryDecoder : Decode.Decoder Geometry
geometryDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "Polygon" ->
                        Decode.field "coordinates" (Decode.list (Decode.list pointDecoder))
                            |> Decode.map Polygon

                    "MultiPolygon" ->
                        Decode.field "coordinates" (Decode.list (Decode.list (Decode.list pointDecoder)))
                            |> Decode.map MultiPolygon

                    "LineString" ->
                        Decode.field "coordinates" (Decode.list pointDecoder)
                            |> Decode.map LineString

                    _ ->
                        Decode.fail ("Unsupported GeoJSON geometry: " ++ kind)
            )


pointDecoder : Decode.Decoder GeoPoint
pointDecoder =
    Decode.map2
        (\longitude latitude -> { latitude = latitude, longitude = longitude })
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)


{-| Convert each geographic edge independently. In particular, an edge crossing
the antimeridian has endpoints that are close in Cartesian space, so it stays
short without longitude interpolation.
-}
borderSegments : Geometry -> List ( Vec3, Vec3 )
borderSegments geometry =
    case geometry of
        Polygon rings ->
            List.concatMap ringSegments rings

        MultiPolygon polygons ->
            polygons |> List.concatMap (List.concatMap ringSegments)

        LineString line ->
            ringSegments line


ringSegments : List GeoPoint -> List ( Vec3, Vec3 )
ringSegments ring =
    let
        vertices = List.map (latitudeLongitudeToUnitVector >> scale 1.003) ring
        adjacent = List.map2 Tuple.pair vertices (List.drop 1 vertices)
        closing =
            case ( List.head vertices, List.reverse vertices |> List.head ) of
                ( Just first, Just last ) ->
                    if sameVector first last then
                        []

                    else
                        [ ( last, first ) ]

                _ ->
                    []
    in
    adjacent ++ closing


sameVector : Vec3 -> Vec3 -> Bool
sameVector first second =
    first.x == second.x && first.y == second.y && first.z == second.z
