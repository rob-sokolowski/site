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
import Html exposing (Attribute, Html, button, div, h2, h3, input, label, li, p, span, text, ul)
import Html.Attributes as A
import Html.Events as E
import Page
import Request
import Shared
import String
import Svg exposing (Svg)
import Svg.Attributes as SA
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> View Msg
view model =
    { title = "Soren's Smart Squeeze"
    , body =
        [ viewBody model ]
    }


{-| The Smart Squeeze — Interactive Micro-Model (Elm)
Now with:
• SVG charts for R(J), Π(J), and p\_eff·J
• Markers at J\* (app) and J\* (direct)
• Scenario presets
-}



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
    , jMaxView : Float
    }


init : ( Model, Effect Msg )
init =
    ( { -- Labs / capability (sets the floor p_eff = p / sigma(K))
        p = 0.003 -- lab price per raw token ($)
      , k = 4.0 -- capability index (improves σ, lowers p_eff)
      , sigmaBase = 1.0
      , sigmaGain = 6.0
      , sigmaSlope = 0.4

      -- App revenue: R(J) = rA * ln(1 + rB * J)
      , rA = 400.0 -- revenue scale (keeps J*app < J*direct)
      , rB = 0.0008 -- growth rate (diminishing returns still hold)

      -- End-user profit: Π(J) = piA * ln(1 + piB * J)
      , piA = 1200.0 -- end-user value scale (ensures Π can beat cost)
      , piB = 0.0012 -- growth rate

      -- App wrapper value: V(J) = vScale * (1 - e^{-vDecay * J})
      , vScale = 40.0 -- total wrapper value ceiling (finite-J noticeable)
      , vDecay = 0.0005 -- marginal wrapper value fades with J (squeeze)

      -- Friction to go direct
      , tau = 1500.0 -- not trivial, not overwhelming

      -- Display / guards
      , jCap = 5.0e6 -- allow larger optima without clipping
      , advanced = False
      , jMaxView = 1.2e6 -- chart x-axis default
      }
    , Effect.none
    )



-- PRESETS


type Preset
    = SmartAppSqueezed
    | DumbMoneyWorkflow
    | HighTauMoat
    | FallingModelPrices
    | HighVPrimeEarly


applyPreset : Preset -> Model -> Model
applyPreset p m =
    case p of
        SmartAppSqueezed ->
            { m
                | p = 0.004
                , k = 7.0
                , sigmaBase = 1.0
                , sigmaGain = 6.0
                , sigmaSlope = 0.4
                , rA = 40.0
                , rB = 0.0016
                , piA = 140.0
                , piB = 0.003
                , vScale = 12.0
                , vDecay = 0.0006
                , tau = 800.0
                , jMaxView = 1.2e6
            }

        DumbMoneyWorkflow ->
            { m
                | p = 0.004
                , k = 5.0
                , sigmaBase = 1.0
                , sigmaGain = 5.0
                , sigmaSlope = 0.35
                , rA = 50.0
                , rB = 0.002
                , piA = 120.0
                , piB = 0.0025
                , vScale = 60.0 -- big wrapper value (workspace/network/trust)
                , vDecay = 0.0002 -- slow decay → sustained V'(J)
                , tau = 6000.0 -- meaningful switching cost
                , jMaxView = 1.0e6
            }

        HighTauMoat ->
            { m
                | p = 0.0035
                , k = 6.0
                , sigmaBase = 1.0
                , sigmaGain = 5.0
                , sigmaSlope = 0.35
                , rA = 55.0
                , rB = 0.002
                , piA = 130.0
                , piB = 0.0028
                , vScale = 18.0
                , vDecay = 0.0007
                , tau = 20000.0 -- very high τ (hard to replace)
                , jMaxView = 1.0e6
            }

        FallingModelPrices ->
            { m
                | p = 0.0012 -- lab price drop
                , k = 8.0 -- capability up
                , sigmaBase = 1.0
                , sigmaGain = 7.0
                , sigmaSlope = 0.45
                , rA = 45.0
                , rB = 0.0022
                , piA = 150.0
                , piB = 0.0032
                , vScale = 14.0
                , vDecay = 0.0006
                , tau = 1200.0
                , jMaxView = 1.5e6
            }

        HighVPrimeEarly ->
            { m
                | p = 0.004
                , k = 5.0
                , sigmaBase = 1.0
                , sigmaGain = 5.0
                , sigmaSlope = 0.35
                , rA = 48.0
                , rB = 0.002
                , piA = 120.0
                , piB = 0.0025
                , vScale = 25.0
                , vDecay = 0.0035 -- wrapper helps a lot early; decays fast
                , tau = 1500.0
                , jMaxView = 6.0e5
            }



-- UPDATE


type Msg
    = SetFloat (Model -> Float) (Float -> Model -> Model) String
    | ToggleAdvanced
    | UsePreset Preset
    | SetJMax String


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

        UsePreset p ->
            ( applyPreset p model, Effect.none )

        SetJMax raw ->
            case String.toFloat raw of
                Just v ->
                    ( { model | jMaxView = clampPos v }, Effect.none )

                Nothing ->
                    ( model, Effect.none )



-- ECON HELPERS


sigma : Model -> Float
sigma m =
    let
        s =
            m.sigmaBase + m.sigmaGain * (1 - (e ^ (-m.sigmaSlope * m.k)))
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
    m.vScale * (1 - (e ^ (-m.vDecay * j)))


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
    div []
        [ h2 [] [ text "The Smart Squeeze — Interactive Micro-Model" ]
        , p []
            [ text "Important note: I didn't do much here, all credit goes to "
            , Html.a [ A.href "https://x.com/hypersoren", A.target "_blank", A.style "color" "#0074d9" ] [ text "@hypersoren" ]
            , text ", his brilliant "
            , Html.a [ A.href "https://hypersoren.xyz/posts/smart-squeeze/", A.target "_blank", A.style "color" "#0074d9" ] [ text "Smart Squeeze" ]
            , text " post, and ChatGPT 5"
            ]
        , p [] [ text "Tweak parameters to see floor/ceiling, optimal J*, margins, disintermediation pressure, and curves." ]
        , presetsBar
        , controls m
        , if m.advanced then
            advancedPanel m

          else
            text ""
        , p []
            [ strong
                (if willDisintermediate then
                    "⚠ End-user goes direct (at their optimum)"

                 else
                    "✅ End-user stays with app (given pricing bound)"
                )
            ]
        , chartPanel m
        , chartExplanation
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
        , p [ A.style "margin-top" "16px", A.style "font-size" "12px", A.style "opacity" "0.8" ]
            [ text "Model forms: σ(K)=σ0+σg(1-e^{-σsK}), R(J)=rA ln(1+rB J), Π(J)=πA ln(1+πB J), V(J)=v(1-e^{-dJ}). Bounds: finite p_app ≤ p/σ + (V(J)+τ)/J; asym p_app ≤ p/σ + V'(J)." ]
        ]


presetsBar : Html Msg
presetsBar =
    row
        [ pill "Smart App Squeezed" (UsePreset SmartAppSqueezed)
        , pill "Dumb Money Workflow" (UsePreset DumbMoneyWorkflow)
        , pill "High-τ Moat" (UsePreset HighTauMoat)
        , pill "Falling Model Prices" (UsePreset FallingModelPrices)
        , pill "High V'(J) Early Only" (UsePreset HighVPrimeEarly)
        ]


controls : Model -> Html Msg
controls m =
    div [ A.style "display" "grid", A.style "grid-template-columns" "repeat(2,minmax(280px,1fr))", A.style "gap" "12px", A.style "margin" "8px 0 16px 0" ]
        [ group "Labs / Capability"
            [ slider "p  (price per raw token, $)" 0.0001 0.02 0.0001 m.p (\s -> SetFloat .p (\v md -> { md | p = v }) s)
            , slider "K  (capability index)" 0 10 0.1 m.k (\s -> SetFloat .k (\v md -> { md | k = v }) s)
            , slider "σ_base" 0.5 5 0.1 m.sigmaBase (\s -> SetFloat .sigmaBase (\v md -> { md | sigmaBase = v }) s)
            , slider "σ_gain" 0 20 0.5 m.sigmaGain (\s -> SetFloat .sigmaGain (\v md -> { md | sigmaGain = v }) s)
            , slider "σ_slope" 0.05 1.0 0.01 m.sigmaSlope (\s -> SetFloat .sigmaSlope (\v md -> { md | sigmaSlope = v }) s)
            ]
        , group "Value & Frictions"
            [ slider "τ  (one-time integration cost, $)" 0 40000 100 m.tau (\s -> SetFloat .tau (\v md -> { md | tau = v }) s)
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
        , div [ A.style "display" "grid", A.style "grid-template-columns" "repeat(2,minmax(280px,1fr))", A.style "gap" "12px" ]
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



-- CHARTS


chartPanel : Model -> Html Msg
chartPanel m =
    let
        w =
            760

        h =
            300

        padL =
            60

        padR =
            20

        padT =
            20

        padB =
            34

        jMax =
            clampPos m.jMaxView |> min m.jCap

        -- sample curve to determine y-max for scaling
        js =
            sampleRange 0 jMax 256

        rs =
            List.map (r m) js

        pis =
            List.map (piFun m) js

        costs =
            List.map (\j -> pEff m * j) js

        yMaxRaw =
            List.maximum (rs ++ pis ++ costs) |> Maybe.withDefault 1

        yMax =
            if yMaxRaw <= 0 then
                1

            else
                yMaxRaw

        toX j =
            let
                x0 =
                    toFloat padL

                x1 =
                    toFloat w - toFloat padR
            in
            x0 + toFloat j / jMax * (x1 - x0)

        toY y =
            let
                y0 =
                    toFloat padT

                y1 =
                    toFloat h - toFloat padB
            in
            -- flip Y for SVG
            y1 - (y / yMax) * (y1 - y0)

        pathOf : (Basics.Float -> Basics.Float) -> String.String
        pathOf f =
            pathFromPoints (List.map (\j -> ( toX j, toY (f <| toFloat j) )) (List.map (\f_ -> round f_) js))

        jA =
            jStarApp m

        jD =
            jStarDirect m

        marker x lbl =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (sf x)
                    , SA.x2 (sf x)
                    , SA.y1 (sf (toFloat padT))
                    , SA.y2 (sf (toFloat h - padB))
                    , SA.stroke "#bbb"
                    , SA.strokeDasharray "4 4"
                    ]
                    []
                , Svg.text_
                    [ SA.x (sf (x + 6)), SA.y (sf (toFloat padT + 14)), SA.fill "#555", SA.fontSize "12px" ]
                    [ Svg.text lbl ]
                ]

        xAxisTicks =
            ticks 5 jMax

        yAxisTicks =
            ticks 5 yMax
    in
    div [ A.style "margin-top" "10px" ]
        [ h3 [] [ text "Curves" ]
        , row
            [ stat "J axis max" (prettyVal jMax)
            , div []
                [ input
                    [ A.type_ "range"
                    , A.min "10000"
                    , A.max "3000000"
                    , A.step "10000"
                    , A.value (String.fromFloat jMax)
                    , E.onInput SetJMax
                    , A.style "width" "240px"
                    ]
                    []
                ]
            ]
        , Svg.svg [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h), SA.width "100%", SA.height "300px", SA.style "background:#fff;border:1px solid #e5e7eb;border-radius:10px" ]
            ([ -- axes
               Svg.line [ SA.x1 (sf padL), SA.y1 (sf (h - padB)), SA.x2 (sf (w - padR)), SA.y2 (sf (h - padB)), SA.stroke "#888" ] []
             , Svg.line [ SA.x1 (sf padL), SA.y1 (sf padT), SA.x2 (sf padL), SA.y2 (sf (h - padB)), SA.stroke "#888" ] []
             ]
                ++ List.map
                    (\t ->
                        let
                            x =
                                toX t
                        in
                        Svg.g []
                            [ Svg.line [ SA.x1 (sf x), SA.x2 (sf x), SA.y1 (sf (h - padB)), SA.y2 (sf (h - padB + 6)), SA.stroke "#888" ] []
                            , Svg.text_ [ SA.x (sf x), SA.y (sf (h - padB + 20)), SA.textAnchor "middle", SA.fill "#666", SA.fontSize "11px" ] [ Svg.text (formatK <| toFloat t) ]
                            ]
                    )
                    (List.map (\f -> round f) xAxisTicks)
                ++ List.map
                    (\t ->
                        let
                            y =
                                toY t
                        in
                        Svg.g []
                            [ Svg.line [ SA.x1 (sf (padL - 6)), SA.x2 (sf padL), SA.y1 (sf y), SA.y2 (sf y), SA.stroke "#888" ] []
                            , Svg.text_ [ SA.x (sf (padL - 10)), SA.y (sf (y + 4)), SA.textAnchor "end", SA.fill "#666", SA.fontSize "11px" ] [ Svg.text ("$" ++ formatK t) ]
                            ]
                    )
                    yAxisTicks
                ++ [ -- curves (retain default colors)
                     Svg.path
                        [ SA.d (pathOf (r m))
                        , SA.fill "none"
                        , SA.strokeWidth "2"
                        , SA.stroke "#1f77b4" -- <— add this
                        ]
                        []
                   , Svg.path
                        [ SA.d (pathOf (piFun m))
                        , SA.fill "none"
                        , SA.strokeWidth "2"
                        , SA.stroke "#2ca02c" -- <— add this
                        ]
                        []
                   , Svg.path
                        [ SA.d (pathOf (\j -> pEff m * j))
                        , SA.fill "none"
                        , SA.strokeWidth "2"
                        , SA.stroke "#555" -- <— add this
                        ]
                        []
                   , marker (toX <| round jA) ("J* app = " ++ formatK jA)
                   , marker (toX <| round jD) ("J* direct = " ++ formatK jD)
                   , legend (padL + 600) (padT + 8)
                   ]
            )
        ]


legend : Int -> Int -> Svg msg
legend x y =
    Svg.g []
        [ Svg.rect
            [ SA.x (si x)
            , SA.y (si y)
            , SA.rx "8"
            , SA.ry "8"
            , SA.width "180"
            , SA.height "72"
            , SA.fill "white"
            , SA.stroke "#e5e7eb"
            ]
            []

        -- R(J)
        , Svg.circle
            [ SA.cx (si (x + 14))
            , SA.cy (si (y + 18))
            , SA.r "4"
            , SA.fill "#1f77b4"
            ]
            []
        , Svg.text_
            [ SA.x (si (x + 28))
            , SA.y (si (y + 22))
            , SA.fontSize "12px"
            , SA.fill "#333"
            ]
            [ Svg.text "R(J)" ]

        -- Π(J)
        , Svg.circle
            [ SA.cx (si (x + 14))
            , SA.cy (si (y + 38))
            , SA.r "4"
            , SA.fill "#2ca02c"
            ]
            []
        , Svg.text_
            [ SA.x (si (x + 28))
            , SA.y (si (y + 42))
            , SA.fontSize "12px"
            , SA.fill "#333"
            ]
            [ Svg.text "Π(J)" ]

        -- p_eff · J
        , Svg.circle
            [ SA.cx (si (x + 14))
            , SA.cy (si (y + 58))
            , SA.r "4"
            , SA.fill "#555555"
            ]
            []
        , Svg.text_
            [ SA.x (si (x + 28))
            , SA.y (si (y + 62))
            , SA.fontSize "12px"
            , SA.fill "#333"
            ]
            [ Svg.text "p_eff · J" ]
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
            , A.style "width" "240px"
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


pill : String -> Msg -> Html Msg
pill label_ msg =
    button
        [ E.onClick msg
        , A.style "border" "1px solid #e5e7eb"
        , A.style "border-radius" "999px"
        , A.style "padding" "6px 10px"
        , A.style "background" "white"
        ]
        [ text label_ ]


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



-- NUMERIC / CHART HELPERS


clampNonNeg : Float -> Float
clampNonNeg x =
    if x < 0 then
        0

    else
        x


clampPos : Float -> Float
clampPos x =
    if x <= 10000 then
        10000

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


formatK : Float -> String
formatK v =
    let
        absV =
            Basics.abs v
    in
    if absV >= 1.0e6 then
        fixed 2 (v / 1.0e6) ++ "M"

    else if absV >= 1.0e3 then
        fixed 2 (v / 1.0e3) ++ "k"

    else
        fixed 2 v


log1p : Float -> Float
log1p x =
    if Basics.abs x < 1.0e-6 then
        x - (x ^ 2) / 2 + (x ^ 3) / 3

    else
        Basics.logBase e (1 + x)


sf : Float -> String
sf =
    String.fromFloat


si : Int -> String
si =
    String.fromInt


sampleRange : Float -> Float -> Int -> List Float
sampleRange a b n =
    if n <= 1 then
        [ a, b ]

    else
        let
            step =
                (b - a) / toFloat (n - 1)
        in
        List.map (\i -> a + toFloat i * step) (List.range 0 (n - 1))


pathFromPoints : List ( Float, Float ) -> String
pathFromPoints pts =
    case pts of
        [] ->
            ""

        ( x0, y0 ) :: rest ->
            "M "
                ++ String.fromFloat x0
                ++ " "
                ++ String.fromFloat y0
                ++ concatMap (\( x, y ) -> " L " ++ String.fromFloat x ++ " " ++ String.fromFloat y) rest


concatMap : (a -> String) -> List a -> String
concatMap f xs =
    String.join "" (List.map f xs)


ticks : Int -> Float -> List Float
ticks n maxV =
    let
        m =
            toFloat (max 1 n)

        step =
            maxV / m
    in
    List.map (\i -> step * toFloat i) (List.range 0 n)


chartExplanation : Html msg
chartExplanation =
    div [ A.style "margin-top" "12px", A.style "font-size" "14px", A.style "line-height" "1.45" ]
        [ h3 [ A.style "margin" "0 0 6px 0" ] [ text "What the chart shows" ]
        , ul [ A.style "margin" "0 0 6px 18px", A.style "padding" "0" ]
            [ li []
                [ span
                    [ A.style "display" "inline-block"
                    , A.style "width" "10px"
                    , A.style "height" "10px"
                    , A.style "border-radius" "9999px"
                    , A.style "background" "#1f77b4"
                    , A.style "margin-right" "8px"
                    ]
                    []
                , strong_ "R(J): "
                , text "App revenue from delivering J units of intelligence — rises fast at first, then flattens (diminishing returns)."
                ]
            , li []
                [ span
                    [ A.style "display" "inline-block"
                    , A.style "width" "10px"
                    , A.style "height" "10px"
                    , A.style "border-radius" "9999px"
                    , A.style "background" "#2ca02c"
                    , A.style "margin-right" "8px"
                    ]
                    []
                , strong_ "Π(J): "
                , text "End-user value from consuming J — also increasing with diminishing returns (typically above R(J))."
                ]
            , li []
                [ span
                    [ A.style "display" "inline-block"
                    , A.style "width" "10px"
                    , A.style "height" "10px"
                    , A.style "border-radius" "9999px"
                    , A.style "background" "#555555"
                    , A.style "margin-right" "8px"
                    ]
                    []
                , strong_ "p_eff · J: "
                , text "Lab cost to supply J (effective price per unit × J) — a straight line from the origin."
                ]
            , li []
                [ span
                    [ A.style "display" "inline-block"
                    , A.style "width" "10px"
                    , A.style "height" "10px"
                    , A.style "border-radius" "2px"
                    , A.style "background" "repeating-linear-gradient(90deg, #bbb 0 6px, transparent 6px 12px)"
                    , A.style "margin-right" "8px"
                    ]
                    []
                , strong_ "Vertical dashed lines (J*): "
                , text "Bookmarks for optimal choices. J* (app) marks where the app’s marginal revenue equals lab marginal cost; J* (direct) marks where the end-user’s marginal value equals lab marginal cost."
                ]
            ]
        , p [ A.style "margin" "6px 0 0 0", A.style "opacity" "0.8" ]
            [ text "Rule of thumb: if J*direct ≫ J*app, the user wants far more intelligence than the app provides — disintermediation pressure is high." ]
        ]



-- small helper for bold inline labels


strong_ : String -> Html msg
strong_ s =
    span [ A.style "font-weight" "600" ] [ text s ]
