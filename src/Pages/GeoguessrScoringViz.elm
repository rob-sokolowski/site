module Pages.GeoguessrScoringViz exposing (Model, Msg, page)

import Browser.Events as BrowserEvents
import Effect exposing (Effect)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geo.Contours as Contours
import Geo.Coordinates as Coordinates exposing (GeoPoint, Vec3)
import Geo.GeoGuessrScore as Score
import Geography.GeoJson as GeoJson
import Gen.Params.GeoguessrScoringViz exposing (Params)
import Html
import Html.Attributes as HtmlAttributes
import Html.Events as HtmlEvents
import Http
import Json.Decode as Decode
import Math.Vector3 as Vector3
import Math.Vector4 as Vector4
import Page
import Request
import Shared
import View exposing (View)
import WebGL


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Vertex =
    { position : Vector3.Vec3 }


type alias Model =
    { target : GeoPoint
    , mapScaleKm : Float
    , contourInterval : Int
    , yaw : Float
    , pitch : Float
    , zoom : Float
    , dragging : Maybe ( Float, Float )
    , borders : List GeoJson.Geometry
    }


initialTarget : GeoPoint
initialTarget =
    { latitude = 40.7128, longitude = -74.0060 }


init : Shared.Model -> ( Model, Effect Msg )
init _ =
    let
        model =
            { mapScaleKm = Score.worldModel.mapScaleKm
            , target = initialTarget
            , contourInterval = 100
            , yaw = 0.55
            , pitch = -0.25
            , zoom = 3.1
            , dragging = Nothing
            , borders = []
            }
    in
    ( model
    , Effect.fromCmd <|
        Http.get
            { url = "/countries.geojson"
            , expect = Http.expectJson GotBorders GeoJson.featureCollectionDecoder
            }
    )


type Msg
    = SetContourInterval Float
    | SetLatitude String
    | SetLongitude String
    | SetMapScale String
    | StartDrag Float Float
    | Drag Float Float
    | EndDrag
    | Zoom Float
    | GotBorders (Result Http.Error (List GeoJson.Geometry))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetContourInterval value ->
            ( { model | contourInterval = round value }, Effect.none )

        SetLatitude value ->
            case String.toFloat value of
                Just latitude ->
                    (
                        { model
                            | target = { latitude = Coordinates.clamp -90 90 latitude, longitude = model.target.longitude }
                        }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        SetLongitude value ->
            case String.toFloat value of
                Just longitude ->
                    (
                        { model
                            | target = { latitude = model.target.latitude, longitude = Coordinates.clamp -180 180 longitude }
                        }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        SetMapScale value ->
            case String.toFloat value of
                Just mapScaleKm ->
                    if mapScaleKm > 0 then
                        ( { model | mapScaleKm = mapScaleKm }, Effect.none )

                    else
                        ( model, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        StartDrag x y ->
            ( { model | dragging = Just ( x, y ) }, Effect.none )

        Drag x y ->
            case model.dragging of
                Just ( previousX, previousY ) ->
                    ( { model
                        | yaw = model.yaw + (x - previousX) * 0.01
                        , pitch = Coordinates.clamp -1.35 1.35 (model.pitch + (y - previousY) * 0.01)
                        , dragging = Just ( x, y )
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        EndDrag ->
            ( { model | dragging = Nothing }, Effect.none )

        Zoom wheelDelta ->
            ( { model | zoom = Coordinates.clamp 1.7 5.0 (model.zoom * (e ^ (wheelDelta * 0.001))) }, Effect.none )

        GotBorders result ->
            case result of
                Ok geometries ->
                    ( { model | borders = geometries }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragging of
        Just _ ->
            Sub.batch
                [ BrowserEvents.onMouseMove mousePositionDecoder
                , BrowserEvents.onMouseUp (Decode.succeed EndDrag)
                ]

        Nothing ->
            Sub.none


mousePositionDecoder : Decode.Decoder Msg
mousePositionDecoder =
    Decode.map2 Drag
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


view : Model -> View Msg
view model =
    { title = "GeoGuessr score globe"
    , body =
        [ E.layout
            [ E.width E.fill
            , E.height E.fill
            , Background.color (E.rgb255 12 18 30)
            , Font.color (E.rgb255 235 243 255)
            ]
            (viewElements model)
        ]
    }


viewElements : Model -> Element Msg
viewElements model =
    E.column
        [ E.width E.fill, E.height E.fill, E.padding 24, E.spacing 18 ]
        [ E.column [ E.spacing 5 ]
            [ E.el [ Font.size 28, Font.bold ] (E.text "GeoGuessr score globe")
            , E.el [ Font.color (E.rgb255 164 188 215) ]
                (E.text "Equal-score contours are small circles on the Earth, not circles on a map.")
            ]
        , E.wrappedRow [ E.width E.fill, E.spacing 24, E.alignTop ]
            [ E.el [ E.width (E.maximum 900 E.fill), E.height (E.px 600) ] (E.html (globe model))
            , controls model
            ]
        ]


controls : Model -> Element Msg
controls model =
    E.column
        [ E.width (E.px 270)
        , E.spacing 16
        , E.padding 18
        , Background.color (E.rgb255 21 31 49)
        , Border.rounded 12
        ]
        [ E.el [ Font.bold, Font.size 18 ] (E.text "Controls")
        , Input.slider [ E.width E.fill ]
            { onChange = SetContourInterval
            , label = Input.labelAbove [ Font.color (E.rgb255 184 203 228) ] (E.text ("Contours: " ++ String.fromInt model.contourInterval ++ " points"))
            , min = 10
            , max = 500
            , step = Just 10
            , value = toFloat model.contourInterval
            , thumb = Input.defaultThumb
            }
        , numberInput "Target latitude" model.target.latitude SetLatitude
        , numberInput "Target longitude" model.target.longitude SetLongitude
        , numberInput "Map scale (km)" model.mapScaleKm SetMapScale
        , E.paragraph [ Font.size 14, Font.color (E.rgb255 164 188 215), E.spacing 6 ]
            [ E.text "Drag to orbit. Use the mouse wheel to zoom. The default 20,000 km map scale is configurable." ]
        ]


numberInput : String -> Float -> (String -> Msg) -> Element Msg
numberInput label value message =
    Input.text [ E.width E.fill ]
        { onChange = message
        , text = String.fromFloat value
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.color (E.rgb255 184 203 228) ] (E.text label)
        }


globe : Model -> Html.Html Msg
globe model =
    let
        uniforms color =
            { yaw = model.yaw
            , pitch = model.pitch
            , zoom = model.zoom
            , aspect = 1.5
            , color = color
            }

        entity mesh color =
            WebGL.entity vertexShader fragmentShader mesh (uniforms color)
    in
    WebGL.toHtmlWith
        [ WebGL.antialias, WebGL.depth 1, WebGL.clearColor 0.035 0.065 0.11 1 ]
        [ HtmlAttributes.width 900
        , HtmlAttributes.height 600
        , HtmlAttributes.style "display" "block"
        , HtmlAttributes.style "width" "100%"
        , HtmlAttributes.style "height" "600px"
        , HtmlAttributes.style "border-radius" "14px"
        , HtmlAttributes.style "cursor" "grab"
        , HtmlAttributes.style "touch-action" "none"
        , HtmlEvents.preventDefaultOn "mousedown"
            (Decode.map (\message -> ( message, True )) mouseDownDecoder)
        , HtmlEvents.onMouseUp EndDrag
        , HtmlEvents.on "wheel" (Decode.field "deltaY" Decode.float |> Decode.map Zoom)
        ]
        (entity sphereMesh (Vector4.vec4 0.07 0.29 0.53 1)
            :: borderEntities model entity
            ++ (entity (targetMesh model.target) (Vector4.vec4 1 0.42 0.12 1)
                    :: (contourMeshes model
                            |> List.map (\mesh -> entity mesh (Vector4.vec4 0.25 0.9 0.95 1))
                       )
               )
        )


borderEntities : Model -> (WebGL.Mesh Vertex -> Vector4.Vec4 -> WebGL.Entity) -> List WebGL.Entity
borderEntities model entity =
    if List.isEmpty model.borders then
        []

    else
        [ model.borders
            |> List.concatMap GeoJson.borderSegments
            |> List.map (\( first, second ) -> ( toVertex first, toVertex second ))
            |> WebGL.lines
            |> (\mesh -> entity mesh (Vector4.vec4 0.55 0.68 0.73 1))
        ]


contourMeshes : Model -> List (WebGL.Mesh Vertex)
contourMeshes model =
    let
        scoreModel =
            { mapScaleKm = model.mapScaleKm, earthRadiusKm = Score.earthRadiusKm }
    in
    Contours.scoreLevels model.contourInterval
        |> List.map (\scoreValue -> Contours.contour scoreModel model.target scoreValue contourSamples)
        |> List.map (List.map toVertex >> WebGL.lineLoop)


mouseDownDecoder : Decode.Decoder Msg
mouseDownDecoder =
    Decode.map2 StartDrag
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


type alias Uniforms =
    { yaw : Float
    , pitch : Float
    , zoom : Float
    , aspect : Float
    , color : Vector4.Vec4
    }


type alias Varyings =
    { shade : Float }


vertexShader : WebGL.Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform float yaw;
        uniform float pitch;
        uniform float zoom;
        uniform float aspect;
        varying float shade;
        void main () {
            float cy = cos(yaw);
            float sy = sin(yaw);
            float cp = cos(pitch);
            float sp = sin(pitch);
            vec3 yawed = vec3(position.x * cy + position.z * sy, position.y, -position.x * sy + position.z * cy);
            vec3 rotated = vec3(yawed.x, yawed.y * cp - yawed.z * sp, yawed.y * sp + yawed.z * cp);
            vec3 normal = normalize(rotated);
            shade = max(0.0, dot(normal, normalize(vec3(-0.4, 0.75, 1.0))));
            float viewZ = rotated.z - zoom;
            float near = 0.1;
            float far = 20.0;
            float focal = 1.55;
            gl_Position = vec4(
                focal * rotated.x / aspect,
                focal * rotated.y,
                ((far + near) / (near - far)) * viewZ + ((2.0 * far * near) / (near - far)),
                -viewZ
            );
            gl_PointSize = 13.0;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        varying float shade;
        void main () {
            gl_FragColor = vec4(color.rgb * (0.42 + 0.58 * shade), color.a);
        }
    |]


latitudeSegments : Int
latitudeSegments =
    48


contourSamples : Int
contourSamples =
    128


longitudeSegments : Int
longitudeSegments =
    96


sphereMesh : WebGL.Mesh Vertex
sphereMesh =
    List.range 0 (latitudeSegments - 1)
        |> List.concatMap
            (\latitudeIndex ->
                List.range 0 (longitudeSegments - 1)
                    |> List.concatMap
                        (\longitudeIndex ->
                            let
                                nextLongitude = modBy longitudeSegments (longitudeIndex + 1)
                                a = sphereVertex latitudeIndex longitudeIndex
                                b = sphereVertex (latitudeIndex + 1) longitudeIndex
                                c = sphereVertex (latitudeIndex + 1) nextLongitude
                                d = sphereVertex latitudeIndex nextLongitude
                            in
                            [ ( a, b, c ), ( a, c, d ) ]
                        )
            )
        |> WebGL.triangles


sphereVertex : Int -> Int -> Vertex
sphereVertex latitudeIndex longitudeIndex =
    let
        latitude = -pi / 2 + pi * toFloat latitudeIndex / toFloat latitudeSegments
        longitude = 2 * pi * toFloat longitudeIndex / toFloat longitudeSegments
    in
    { position = Vector3.vec3 (cos latitude * cos longitude) (sin latitude) (cos latitude * sin longitude) }


targetMesh : GeoPoint -> WebGL.Mesh Vertex
targetMesh target =
    target
        |> Coordinates.latitudeLongitudeToUnitVector
        |> scaleVector 1.035
        |> toVertex
        |> List.singleton
        |> WebGL.points


toVertex : Vec3 -> Vertex
toVertex vector =
    { position = Vector3.vec3 vector.x vector.y vector.z }


scaleVector : Float -> Vec3 -> Vec3
scaleVector amount vector =
    { x = amount * vector.x, y = amount * vector.y, z = amount * vector.z }
