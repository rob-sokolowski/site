module Pages.IkedaPattern exposing (Model, Msg, page, primes, quadraticResidueSet)

import Array2D exposing (Array2D)
import Basics
import Browser.Dom
import Browser.Events as BrowserEvents
import Dict
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.IkedaPattern exposing (Params)
import List.Extra as List
import Page
import Palette exposing (toAvhColor)
import Request
import Set exposing (Set)
import Shared
import Task
import Time
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC exposing (Svg)
import TypedSvg.Types as ST exposing (Transform(..))
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        -- parse query param ?page=
        pageNo : Int
        pageNo =
            Dict.get "page" req.query
                |> Maybe.withDefault ""
                |> String.toInt
                |> Maybe.withDefault 1
    in
    Page.advanced
        { init = init shared pageNo
        , update = update_
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { viewportStatus : ViewportStatus
    , pattern : Array2D ( Float, Float, Color )
    , rotDeg : Float -- rotation to apply, in degrees
    , n : Int
    , pageNo : Int
    }


page1N0 =
    30


page2N0 =
    53


page3N0 =
    101


init : Shared.Model -> Int -> ( Model, Effect Msg )
init shared pageNo =
    let
        ( n0, pattern ) =
            case pageNo of
                3 ->
                    ( page3N0, muraPattern2 page3N0 )

                2 ->
                    ( page2N0, muraPattern page2N0 )

                _ ->
                    ( page1N0, checkeredPattern page1N0 )
    in
    ( { viewportStatus = ViewportUnknown
      , pattern = pattern
      , rotDeg = rotDeg0
      , n = n0
      , pageNo = pageNo
      }
    , Effect.fromCmd (Task.perform Got_Viewport Browser.Dom.getViewport)
    )



-- UPDATE


type ViewportStatus
    = ViewportUnknown
    | ViewportKnown Browser.Dom.Viewport


type Msg
    = Got_Viewport Browser.Dom.Viewport
    | Got_ResizeEvent Int Int
    | Tick Float


update_ : Msg -> Model -> ( Model, Effect Msg )
update_ msg model =
    case model.viewportStatus of
        ViewportUnknown ->
            case msg of
                Got_Viewport viewport ->
                    ( { model | viewportStatus = ViewportKnown viewport }, Effect.none )

                _ ->
                    ( model, Effect.none )

        --_ ->
        --    ( model, Effect.none )
        ViewportKnown viewport ->
            update msg ( model, viewport )


tickPage1 : Model -> Float -> ( Model, Effect Msg )
tickPage1 model dt =
    let
        rotDeg : Float
        rotDeg =
            -- continue rotation, mod 360 to avoid extraneous rotations (540 degrees is same as 180, for example)
            toFloat <| modBy 360 (round <| model.rotDeg + dTheta dt)

        a : Float
        a =
            -- defines the "amplitude" of the sinusoidal function that determines n
            20

        n_ : Int
        n_ =
            -- the dimension of the checkered pattern varies sinusoidally, with an amplitude of a, centered at n0
            round <| page1N0 + (a * Basics.sin (degrees rotDeg))

        -- recompute pattern if n has changed, otherwise don't bother since it'll be the same
        pattern : Array2D ( Basics.Float, Basics.Float, Color )
        pattern =
            if model.n /= n_ then
                checkeredPattern n_

            else
                model.pattern
    in
    ( { model
        | rotDeg = rotDeg
        , n = n_
        , pattern = pattern
      }
    , Effect.none
    )


tickPage3 : Model -> Float -> ( Model, Effect Msg )
tickPage3 model dt =
    let
        rotDeg : Float
        rotDeg =
            -- continue rotation, mod 360 to avoid extraneous rotations (540 degrees is same as 180, for example)
            toFloat <| modBy 360 (round <| model.rotDeg + dTheta dt)
    in
    ( { model
        | rotDeg = rotDeg
      }
    , Effect.none
    )


tickPage2 : Model -> Float -> ( Model, Effect Msg )
tickPage2 model dt =
    let
        rotDeg : Float
        rotDeg =
            -- continue rotation, mod 360 to avoid extraneous rotations (540 degrees is same as 180, for example)
            toFloat <| modBy 360 (round <| model.rotDeg + dTheta dt)
    in
    ( { model
        | rotDeg = rotDeg
      }
    , Effect.none
    )


update : Msg -> ( Model, Browser.Dom.Viewport ) -> ( Model, Effect Msg )
update msg ( model, viewport ) =
    case msg of
        Got_Viewport viewport_ ->
            ( { model | viewportStatus = ViewportKnown viewport_ }, Effect.none )

        Got_ResizeEvent _ _ ->
            ( model
            , Effect.fromCmd <| Task.perform Got_Viewport Browser.Dom.getViewport
            )

        Tick dt ->
            case model.pageNo of
                1 ->
                    tickPage1 model dt

                2 ->
                    tickPage2 model dt

                3 ->
                    tickPage3 model dt

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS
-- begin region: constants


rotDeg0 : Float
rotDeg0 =
    -- initial rotation of the pattern
    0.0


dx : number
dx =
    -- The number of pixels, in viewbox coordinates of a square (smaller square components of the checkered pattern)
    10


dTheta : Float -> Float
dTheta dt =
    -- The rotation, in degrees, to apply each frame
    dt / 30.0



-- end region: constants


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BrowserEvents.onResize Got_ResizeEvent
        , BrowserEvents.onAnimationFrameDelta Tick
        ]



-- VIEW


view : Model -> View Msg
view model =
    { title = "Ikeda Pattern"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (case model.viewportStatus of
                ViewportUnknown ->
                    E.none

                ViewportKnown viewport ->
                    viewElements ( model, viewport )
            )
        ]
    }


muraPattern2 : Int -> Array2D ( Float, Float, Color )
muraPattern2 d =
    let
        quadraticResidues : Set Int
        quadraticResidues =
            quadraticResidueSet d

        c : Int -> Int
        c k =
            if Set.member k quadraticResidues then
                1

            else
                -1

        a : ( Int, Int ) -> Int
        a ( i, j ) =
            if i == 0 then
                0

            else if j == 0 && i /= 0 then
                1

            else if c i * c j == 1 then
                1

            else
                0
    in
    Array2D.fromListOfLists <|
        List.map
            (\i ->
                List.map
                    (\j ->
                        if a ( i, j ) == 0 then
                            ( dx * toFloat i, dx * toFloat j, Palette.white )

                        else
                            ( dx * toFloat i, dx * toFloat j, Palette.black )
                    )
                    (List.range 1 d)
            )
            (List.range 1 d)


muraPattern : Int -> Array2D ( Float, Float, Color )
muraPattern d =
    let
        quadraticResidues : Set Int
        quadraticResidues =
            quadraticResidueSet d

        c : Int -> Int
        c k =
            if Set.member k quadraticResidues then
                1

            else
                -1

        a : ( Int, Int ) -> Int
        a ( i, j ) =
            if i == 0 then
                0

            else if j == 0 && i /= 0 then
                1

            else if c i * c j == 1 then
                1

            else
                0
    in
    Array2D.fromListOfLists <|
        List.map
            (\i ->
                List.map
                    (\j ->
                        if a ( i, j ) == 0 then
                            ( dx * toFloat i, dx * toFloat j, Palette.white )

                        else
                            ( dx * toFloat i, dx * toFloat j, Palette.black )
                    )
                    (List.range 1 d)
            )
            (List.range 1 d)


checkeredPattern : Int -> Array2D ( Float, Float, Color )
checkeredPattern n_ =
    Array2D.fromListOfLists <|
        List.map
            (\i ->
                List.map
                    (\j ->
                        if modBy 2 (i + j) == 0 then
                            ( dx * toFloat i, dx * toFloat j, Palette.white )

                        else
                            ( dx * toFloat i, dx * toFloat j, Palette.black )
                    )
                    (List.range 0 (n_ - 1))
            )
            (List.range 0 (n_ - 1))


viewElements : ( Model, Browser.Dom.Viewport ) -> Element Msg
viewElements ( model, viewport ) =
    let
        ( w, h ) =
            ( viewport.viewport.width - 10, viewport.viewport.height - 10 )

        ( vb_w, vb_h ) =
            ( toFloat model.n * dx, toFloat model.n * dx )

        square : ( Float, Float, Color ) -> Svg Msg
        square ( x, y, color ) =
            -- TODO: Need to think about how to paginate the transform operations in a nicer way
            -- For now this case statement will do
            S.rect
                [ SA.x (ST.px <| x)
                , SA.y (ST.px <| y)
                , SA.width (ST.px dx)
                , SA.height (ST.px dx)
                , SA.fill (ST.Paint <| toAvhColor color)
                , SA.stroke (ST.Paint <| toAvhColor color)
                , case model.pageNo of
                    1 ->
                        SA.transform
                            [ Rotate model.rotDeg (vb_w / 2 + dx / 2 + 0.1 * y) (vb_h / 2 + dx / 2 + 0.1 * x)
                            ]

                    2 ->
                        SA.transform
                            [ Rotate model.rotDeg (vb_w / 2 + dx) (vb_h / 2 + dx / 2)
                            ]

                    3 ->
                        SA.transform
                            [ Rotate model.rotDeg (vb_w / 2 + dx) (vb_h / 2 + dx / 2)
                            ]

                    _ ->
                        SA.noFill
                ]
                []
    in
    el
        [ width (px <| round w)
        , height (px <| round h)
        , centerX
        , centerY
        , Border.width 1
        , Border.color Palette.black
        ]
        (E.html <|
            S.svg
                [ SA.width (ST.px w)
                , SA.height (ST.px h)
                , SA.viewBox 0 0 vb_w vb_h
                , SA.color (toAvhColor Palette.white)
                ]
                (List.map (\p -> square p) (Array2D.flattenAsList model.pattern))
        )



-- begin region: prime number stuff


primes : List Int
primes =
    -- source: https://en.wikipedia.org/wiki/List_of_prime_numbers
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101 ]


primeSet : Set Int
primeSet =
    Set.fromList primes


{-| Compute the quadratic residue set of a prime number p. Returns Set.empty when p is not in the above list of primes.
While this list is incomplete, it should be sufficient for the purposes of drawing mura patterns

<https://en.wikipedia.org/wiki/Quadratic_residue>

-}
quadraticResidueSet : Int -> Set Int
quadraticResidueSet p =
    let
        intsLessThanP : List Int
        intsLessThanP =
            List.range 1 (p - 1)

        res : Int -> Int
        res x =
            modBy p (x * x)

        quadraticResidue_ : List Int -> Set Int -> Set Int
        quadraticResidue_ intsRemaining accum =
            case intsRemaining of
                x :: xs ->
                    Set.insert (res x) (quadraticResidue_ xs accum)

                [] ->
                    accum
    in
    if Set.member p primeSet then
        quadraticResidue_ intsLessThanP Set.empty

    else
        Set.empty



-- end region: prime number stuff
