module Pages.SmartSqueeze exposing (Model, Msg, page)

--The Smart Squeeze — Interactive Micro-Model (Elm)
--
--Maps post variables → Elm:
--
--  I          : raw tokens (implicit via J and sigma)
--  K          : model capability index (slider)
--  sigma(K)   : effective intelligence per token (concave down, ↑ with K, diminishing returns)
--  p          : lab price per token (slider)
--  J          : effective intelligence units (decision variable)
--  R(J)       : app revenue from delivering J (increasing, concave down)
--  Π(J)       : end-user profit from J (increasing, concave down)
--  V(J)       : app value-add (increasing, concave down; V′(J) > 0 but ↓ toward 0)
--  τ          : one-time integration cost to go direct (slider)
--
--App chooses J to maximize:
--  π_app(J) = R(J) - (p / sigma(K)) * J
--
--User can go direct, bounding app price per unit:
--  p_app ≤ p/σ(K) + (V(J) + τ)/J     (finite-J bound)
--
--Asymptotically (large J): p_app ≤ p/σ(K) + V′(J)
--
--This app:
--  • Picks closed-form J* from FOC R′(J) = p/σ(K) (with our R form)
--  • Computes price floor, ceiling, margin, app profit
--  • Compares end-user profit via app vs optimal direct J_direct*
--  • Lets you change functional parameters for σ, R, Π, V
--
--Default functional forms (simple, increasing, concave down, closed-form FOCs):
--
--  σ(K)  = σ_base + σ_gain * (1 - e^{-σ_slope K})          (↑, concave down)
--  R(J)  = rA * ln(1 + rB J)                               (↑, concave down)
--  Π(J)  = piA * ln(1 + piB J)                             (↑, concave down)
--  V(J)  = vScale * (1 - e^{-vDecay J})                    (↑, concave down, V′(J) > 0, V′(J) → 0)
--
--FOCs / closed forms:
--  R′(J)  = rA * rB / (1 + rB J)
--  ⇒ R′(J*) = p_eff   where p_eff = p / σ(K)
--  ⇒ J* = max(0, (rA*rB / p_eff - 1) / rB)
--
--  Π′(J)  = piA * piB / (1 + piB J)
--  ⇒ J_direct* = max(0, (piA*piB / p_eff - 1) / piB)
--
--Notes:
--  • If J* ≈ 0, finite bound (V(J)+τ)/J blows up; UI shows "∞".
--  • We clamp negatives to 0 where economically necessary.

import Basics exposing (e)
import Browser
import Effect exposing (Effect)
import Gen.Params.SmartSqueeze exposing (Params)
import Html exposing (Attribute, Html, button, div, h2, h3, input, label, p, span, text)
import Html.Attributes as A
import Html.Events as E
import Page
import Request
import Shared
import String
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { p : Float
    , k : Float
    , sigmaBase : Float
    , sigmaGain : Float
    , sigmaSlope : Float
    , rA : Float
    , rB : Float
    , piA : Float
    , piB : Float
    , vScale : Float
    , vDecay : Float
    , tau : Float
    , jCap : Float
    , advanced : Bool
    }


init : ( Model, Effect Msg )
init =
    ( { -- Labs / capability
        p = 0.004 -- price per raw token ($)
      , k = 5.0 -- capability index (0..10)
      , sigmaBase = 1.0
      , sigmaGain = 5.0
      , sigmaSlope = 0.35

      -- App revenue R(J) = rA * ln(1 + rB J)
      , rA = 50.0
      , rB = 0.002

      -- End-user Π(J) = piA * ln(1 + piB J)
      , piA = 120.0
      , piB = 0.0025

      -- App value-add V(J) = vScale * (1 - e^{-vDecay J})
      , vScale = 20.0
      , vDecay = 0.0008

      -- Integration friction to go direct
      , tau = 3000.0

      -- display caps / guards
      , jCap = 2.0e6
      , advanced = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = SetFloat (Model -> Float) (Float -> Model -> Model) String
    | ToggleAdvanced


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetFloat _ setter raw ->
            case String.toFloat raw of
                Just v ->
                    ( setter v model, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        ToggleAdvanced ->
            ( { model | advanced = not model.advanced }, Effect.none )



-- ECON HELPERS


sigma : Model -> Float
sigma m =
    let
        s =
            m.sigmaBase + m.sigmaGain * (1 - e ^ (-m.sigmaSlope * m.k))
    in
    max 1.0e-9 s


pEff : Model -> Float
pEff m =
    m.p / sigma m


r : Model -> Float -> Float
r m j =
    m.rA * log1p (m.rB * j)


rPrime : Model -> Float -> Float
rPrime m j =
    m.rA * m.rB / (1 + m.rB * j)


piFun : Model -> Float -> Float
piFun m j =
    m.piA * log1p (m.piB * j)


piPrime : Model -> Float -> Float
piPrime m j =
    m.piA * m.piB / (1 + m.piB * j)


vFun : Model -> Float -> Float
vFun m j =
    m.vScale * (1 - e ^ (-m.vDecay * j))


vPrime : Model -> Float -> Float
vPrime m j =
    m.vScale * m.vDecay * (e ^ (-m.vDecay * j))



-- OPTIMA (closed form with our function choices)


jStarApp : Model -> Float
jStarApp m =
    let
        pe =
            pEff m

        num =
            (m.rA * m.rB) / max 1.0e-12 pe - 1
    in
    clampNonNeg (num / m.rB) |> min m.jCap


jStarDirect : Model -> Float
jStarDirect m =
    let
        pe =
            pEff m

        num =
            (m.piA * m.piB) / max 1.0e-12 pe - 1
    in
    clampNonNeg (num / m.piB) |> min m.jCap



-- BOUNDS & PROFITS


finiteBoundMargin : Model -> Float -> Maybe Float
finiteBoundMargin m j =
    if j <= 1.0e-9 then
        Nothing

    else
        Just ((vFun m j + m.tau) / j)


asymBoundMargin : Model -> Float -> Float
asymBoundMargin m j =
    max 0 (vPrime m j)


chosenPapp : Model -> Float -> Float
chosenPapp m j =
    let
        base =
            pEff m

        finite =
            finiteBoundMargin m j
    in
    case finite of
        Just margin ->
            base + margin

        Nothing ->
            base + asymBoundMargin m j


appProfitAt : Model -> Float -> Float
appProfitAt m j =
    let
        margin =
            chosenPapp m j - pEff m
    in
    max 0 margin * j


euProfitViaAppAt : Model -> Float -> Float
euProfitViaAppAt m j =
    let
        pApp =
            chosenPapp m j
    in
    piFun m j + vFun m j - pApp * j


euProfitDirectAt : Model -> Float -> Float
euProfitDirectAt m j =
    piFun m j - pEff m * j - m.tau



-- VIEW
--main : Program () Model Msg
--main =
--    Browser.sandbox { init = init, update = update, view = view }
--


view : Model -> View Msg
view model =
    { title = "Soren's Smart Squeeze"
    , body =
        [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody m =
    let
        pe =
            pEff m

        jA =
            jStarApp m

        jD =
            jStarDirect m

        sigmaVal =
            sigma m

        pApp =
            chosenPapp m jA

        marginFiniteStr =
            case finiteBoundMargin m jA of
                Just x ->
                    "$" ++ fixed 4 x

                Nothing ->
                    "∞"

        boundAsymStr =
            "$" ++ fixed 4 (asymBoundMargin m jA)

        euViaApp =
            euProfitViaAppAt m jA

        euDirOpt =
            euProfitDirectAt m jD

        willDisintermediate =
            euDirOpt > euViaApp + 1.0e-6
    in
    div [ A.style "padding" "10px" ]
        [ h2 [] [ text "The Smart Squeeze — Interactive Micro-Model" ]
        , p []
            [ text "Import note: I didn't do shit here, all credit goes to "
            , Html.a [ A.href "https://x.com/hypersoren", A.target "_blank", A.style "color" "#0074d9" ] [ text "@hypersoren" ]
            , text ", his brilliant "
            , Html.a [ A.href "https://hypersoren.xyz/posts/smart-squeeze/", A.target "_blank", A.style "color" "#0074d9" ] [ text "Smart Squeeze" ]
            , text " post, and ChatGPT 5"
            ]
        , p [] [ text "Tweak parameters to see floor/ceiling, optimal J*, margins, and disintermediation pressure. Read the post linked above if that doesn't make sense to you" ]
        , controls m
        , h3 [] [ text "Derived (at app optimum J*)" ]
        , row
            [ stat "σ(K)" (fixed 3 sigmaVal)
            , stat "p_eff = p / σ(K)" ("$" ++ fixed 6 pe)
            , stat "J* (app)" (fixed 0 jA)
            , stat "J* (direct)" (fixed 0 jD)
            ]
        , row
            [ stat "App price floor" ("$" ++ fixed 6 pe)
            , stat "Ceiling (finite-J): p_app − floor" marginFiniteStr
            , stat "Ceiling (asym): V'(J*)" boundAsymStr
            , stat "Chosen p_app" ("$" ++ fixed 6 pApp)
            ]
        , row
            [ stat "App margin per unit" ("$" ++ fixed 6 (max 0 (pApp - pe)))
            , stat "App profit = margin×J*" ("$" ++ fixed 2 (appProfitAt m jA))
            , stat "EU profit via app @J*" ("$" ++ fixed 2 euViaApp)
            , stat "EU profit direct @J*_direct" ("$" ++ fixed 2 euDirOpt)
            ]
        , p []
            [ strong
                (if willDisintermediate then
                    "⚠ End-user goes direct (at their optimum)"

                 else
                    "✅ End-user stays with app (given pricing bound)"
                )
            ]
        , if m.advanced then
            advancedPanel m

          else
            text ""
        , p [ A.style "margin-top" "16px", A.style "font-size" "12px", A.style "opacity" "0.8" ]
            [ text "Model forms: σ(K)=σ0+σg(1-e^{-σsK}), R(J)=rA ln(1+rB J), Π(J)=πA ln(1+πB J), V(J)=v(1-e^{-dJ}). Bounds: finite p_app ≤ p/σ + (V(J)+τ)/J; asym p_app ≤ p/σ + V'(J)." ]
        ]


controls : Model -> Html Msg
controls m =
    div [ A.style "display" "grid", A.style "grid-template-columns" "repeat(2,minmax(260px,1fr))", A.style "gap" "12px", A.style "margin" "8px 0 16px 0" ]
        [ group "Labs / Capability"
            [ slider "p  (price per raw token, $)" 0.0001 0.02 0.0001 m.p (\s -> SetFloat .p (\v md -> { md | p = v }) s)
            , slider "K  (capability index)" 0 10 0.1 m.k (\s -> SetFloat .k (\v md -> { md | k = v }) s)
            , slider "σ_base" 0.5 5 0.1 m.sigmaBase (\s -> SetFloat .sigmaBase (\v md -> { md | sigmaBase = v }) s)
            , slider "σ_gain" 0 20 0.5 m.sigmaGain (\s -> SetFloat .sigmaGain (\v md -> { md | sigmaGain = v }) s)
            , slider "σ_slope" 0.05 1.0 0.01 m.sigmaSlope (\s -> SetFloat .sigmaSlope (\v md -> { md | sigmaSlope = v }) s)
            ]
        , group "Value & Frictions"
            [ slider "τ  (one-time integration cost, $)" 0 20000 100 m.tau (\s -> SetFloat .tau (\v md -> { md | tau = v }) s)
            , slider "V scale (vScale)" 0 200 1 m.vScale (\s -> SetFloat .vScale (\v md -> { md | vScale = v }) s)
            , slider "V decay (vDecay)" 0.0001 0.01 0.0001 m.vDecay (\s -> SetFloat .vDecay (\v md -> { md | vDecay = v }) s)
            , button [ A.style "margin-top" "12px", E.onClick ToggleAdvanced ]
                [ text
                    (if m.advanced then
                        "Hide advanced R, Π params"

                     else
                        "Show advanced R, Π params"
                    )
                ]
            ]
        ]


advancedPanel : Model -> Html Msg
advancedPanel m =
    div []
        [ h3 [] [ text "Advanced: Functional Parameters" ]
        , div [ A.style "display" "grid", A.style "grid-template-columns" "repeat(2,minmax(260px,1fr))", A.style "gap" "12px" ]
            [ group "App revenue R(J) = rA ln(1 + rB J)"
                [ slider "rA" 1 200 1 m.rA (\s -> SetFloat .rA (\v md -> { md | rA = v }) s)
                , slider "rB" 0.0001 0.01 0.0001 m.rB (\s -> SetFloat .rB (\v md -> { md | rB = v }) s)
                ]
            , group "End-user Π(J) = πA ln(1 + πB J)"
                [ slider "πA" 1 300 1 m.piA (\s -> SetFloat .piA (\v md -> { md | piA = v }) s)
                , slider "πB" 0.0001 0.01 0.0001 m.piB (\s -> SetFloat .piB (\v md -> { md | piB = v }) s)
                ]
            ]
        ]



-- VIEW HELPERS


group : String -> List (Html Msg) -> Html Msg
group title items =
    div [ A.style "border" "1px solid #e5e7eb", A.style "border-radius" "12px", A.style "padding" "12px" ]
        [ h3 [ A.style "margin" "0 0 8px 0", A.style "font-size" "16px" ] [ text title ]
        , div [] items
        ]


slider : String -> Float -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
slider label_ minV maxV stepV val mkMsg =
    div [ A.style "display" "grid", A.style "grid-template-columns" "1fr auto", A.style "align-items" "center", A.style "gap" "8px", A.style "margin" "6px 0" ]
        [ div []
            [ label [] [ text (label_ ++ "  ") ]
            , span [ A.style "font-variant-numeric" "tabular-nums", A.style "opacity" "0.8" ] [ text (prettyVal val) ]
            ]
        , input
            [ A.type_ "range"
            , A.min (String.fromFloat minV)
            , A.max (String.fromFloat maxV)
            , A.step (String.fromFloat stepV)
            , A.value (String.fromFloat val)
            , E.onInput mkMsg
            , A.style "width" "220px"
            ]
            []
        ]


stat : String -> String -> Html msg
stat k v =
    div [ A.style "border" "1px solid #e5e7eb", A.style "border-radius" "10px", A.style "padding" "10px", A.style "min-width" "180px" ]
        [ div [ A.style "font-size" "12px", A.style "opacity" "0.7" ] [ text k ]
        , div [ A.style "font-size" "18px", A.style "font-variant-numeric" "tabular-nums" ] [ text v ]
        ]


row : List (Html msg) -> Html msg
row kids =
    div [ A.style "display" "flex", A.style "flex-wrap" "wrap", A.style "gap" "10px", A.style "margin" "8px 0" ] kids


strong : String -> Html msg
strong s =
    span [ A.style "font-weight" "600" ] [ text s ]


styles =
    { container =
        A.style "max-width" "1100px"
            :: A.style "margin" "20px auto"
            :: A.style "padding" "0 12px"
            :: []
    }



-- NUMERIC HELPERS


clampNonNeg : Float -> Float
clampNonNeg x =
    if x < 0 then
        0

    else
        x


fixed : Int -> Float -> String
fixed n x =
    let
        pow =
            toFloat (10 ^ n)

        y =
            toFloat (round (x * pow)) / pow
    in
    String.fromFloat y


prettyVal : Float -> String
prettyVal v =
    let
        absV =
            Basics.abs v
    in
    if absV >= 1000000 then
        fixed 2 (v / 1.0e6) ++ "M"

    else if absV >= 1000 then
        fixed 2 (v / 1.0e3) ++ "k"

    else if absV >= 1 then
        fixed 3 v

    else
        fixed 6 v


log1p : Float -> Float
log1p x =
    -- numerically stable ln(1+x)
    if Basics.abs x < 1.0e-6 then
        x - (x ^ 2) / 2 + (x ^ 3) / 3

    else
        Basics.logBase e (1 + x)
