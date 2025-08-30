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
import Html exposing (Attribute, Html, button, div, h2, h3, h4, input, label, li, p, span, text, ul)
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
    { title = "The Smart Squeeze: Watch Apps Get Crushed"
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
    , activeStory : Maybe StoryMode
    , storyStep : Int
    , controlsExpanded : Bool
    }


type StoryMode
    = ModelEvolution
    | PriceWar
    | MoatErosion


init : ( Model, Effect Msg )
init =
    ( { -- Labs / capability (sets the floor p_eff = p / sigma(K))
        p = 0.003 -- lab price per raw token ($)
      , k = 4.0 -- capability index (improves σ, lowers p_eff)
      , sigmaBase = 1.0
      , sigmaGain = 6.0
      , sigmaSlope = 0.4

      -- App revenue: R(J) = rA * ln(1 + rB * J)
      , rA = 800.0 -- revenue scale (closer to user value)
      , rB = 0.001 -- growth rate (diminishing returns still hold)

      -- End-user profit: Π(J) = piA * ln(1 + piB * J)
      , piA = 1000.0 -- end-user value scale (not too much higher than rA)
      , piB = 0.001 -- growth rate

      -- App wrapper value: V(J) = vScale * (1 - e^{-vDecay * J})
      , vScale = 200.0 -- high wrapper value to make app attractive
      , vDecay = 0.0001 -- slower decay so value persists

      -- Friction to go direct
      , tau = 5000.0 -- high integration cost

      -- Display / guards
      , jCap = 50.0e6 -- allow larger optima without clipping
      , advanced = False
      , activeStory = Nothing
      , storyStep = 0
      , controlsExpanded = False
      }
    , Effect.none
    )





-- UPDATE


type Msg
    = SetFloat (Model -> Float) (Float -> Model -> Model) String
    | ToggleAdvanced
    | StartStory StoryMode
    | StepStory
    | StopStory
    | ToggleControls


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
        
        StartStory mode ->
            ( storyStepModel mode 0 { model | activeStory = Just mode, storyStep = 0 }, Effect.none )
        
        StepStory ->
            case model.activeStory of
                Just mode ->
                    let
                        nextStep =
                            model.storyStep + 1
                        
                        maxSteps =
                            4
                        
                        updatedModel =
                            if nextStep < maxSteps then
                                storyStepModel mode nextStep model
                            else
                                { model | activeStory = Nothing, storyStep = 0 }
                    in
                    ( updatedModel, Effect.none )
                
                Nothing ->
                    ( model, Effect.none )
        
        StopStory ->
            ( { model | activeStory = Nothing, storyStep = 0 }, Effect.none )
        
        ToggleControls ->
            ( { model | controlsExpanded = not model.controlsExpanded }, Effect.none )



storyStepModel : StoryMode -> Int -> Model -> Model
storyStepModel mode step model =
    let
        updatedModel =
            { model | storyStep = step }
    in
    case mode of
        ModelEvolution ->
            -- Show AI improvement effect ONLY (need higher K to trigger flip)
            case step of
                0 ->
                    { updatedModel | k = 2.0 }  -- Moderate AI
                
                1 ->
                    { updatedModel | k = 4.0 }  -- Better AI
                
                2 ->
                    { updatedModel | k = 6.0 }  -- Advanced AI
                
                3 ->
                    { updatedModel | k = 8.0 }  -- Very advanced AI (should flip)
                
                _ ->
                    updatedModel
        
        PriceWar ->
            -- Step p from 0.01→0.005→0.002→0.0005
            case step of
                0 ->
                    { updatedModel | p = 0.01 }
                
                1 ->
                    { updatedModel | p = 0.005 }
                
                2 ->
                    { updatedModel | p = 0.002 }
                
                3 ->
                    { updatedModel | p = 0.0005 }
                
                _ ->
                    updatedModel
        
        MoatErosion ->
            -- Show integration cost reduction
            case step of
                0 ->
                    { updatedModel | tau = 8000 }   -- High integration cost
                
                1 ->
                    { updatedModel | tau = 4000 }   -- Dropping
                
                2 ->
                    { updatedModel | tau = 2000 }   -- Getting easier
                
                3 ->
                    { updatedModel | tau = 500 }    -- Very low cost
                
                _ ->
                    updatedModel


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
    -- The app charges based on its revenue function R(J)
    -- Price = Revenue/Quantity = R(J)/J
    if j > 0 then
        r m j / j
    else
        pEff m


appProfitAt : Model -> Float -> Float
appProfitAt m j =
    let
        margin =
            chosenPapp m j - pEff m
    in
    max 0 margin * j


profitAt : Model -> Float -> Float
profitAt m j =
    r m j - pEff m * j


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
        [ h2 [] [ text "The Smart Squeeze: Watch Apps Get Crushed" ]
        , p []
            [ text "Important note: I didn't do much here, all credit goes to "
            , Html.a [ A.href "https://x.com/hypersoren", A.target "_blank", A.style "color" "#0074d9" ] [ text "@hypersoren" ]
            , text ", his brilliant "
            , Html.a [ A.href "https://hypersoren.xyz/posts/smart-squeeze/", A.target "_blank", A.style "color" "#0074d9" ] [ text "Smart Squeeze" ]
            , text " post, and ChatGPT 5"
            ]
        , p [] [ text "Tweak parameters to see floor/ceiling, optimal J*, margins, disintermediation pressure, and curves." ]
        , storyBar m
        , if m.activeStory /= Nothing then
            storyModeIndicator m
          else
            text ""
        , squeezeVisualization m
        , disintermediationDecision m
        , div []
            [ button 
                [ E.onClick ToggleControls
                , A.style "margin" "16px 0 8px 0"
                , A.style "padding" "8px 16px"
                , A.style "background" "#f0f0f0"
                , A.style "border" "1px solid #ddd"
                , A.style "border-radius" "4px"
                , A.style "cursor" "pointer"
                , A.style "font-size" "14px"
                , A.style "display" "flex"
                , A.style "align-items" "center"
                , A.style "gap" "8px"
                ]
                [ text (if m.controlsExpanded then "▼" else "▶")
                , text "Parameter Controls"
                ]
            , if m.controlsExpanded then
                controls m
              else
                text ""
            ]
        , if m.advanced && m.controlsExpanded then
            advancedPanel m

          else
            text ""
        , chartPanel m
        , squeezeMeter m
        ]



storyModeIndicator : Model -> Html Msg
storyModeIndicator m =
    let
        (paramName, currentValue, watchFor) =
            case m.activeStory of
                Just ModelEvolution ->
                    ( "Model Capability (K)"
                    , fixed 1 m.k
                    , "Watch: Intelligence gap grows, margins shrink, app gets squeezed"
                    )
                
                Just PriceWar ->
                    ( "Token Price (p)"
                    , "$" ++ fixed 4 m.p
                    , "Watch: Lower prices → more intelligence demanded → bigger gaps"
                    )
                
                Just MoatErosion ->
                    ( "Integration Cost (τ)"
                    , "$" ++ formatK m.tau
                    , "Watch: Lower setup costs make going direct more attractive"
                    )
                
                _ ->
                    ( "", "", "" )
    in
    div [ A.style "background" "#fef3c7", A.style "border" "2px solid #f59e0b", A.style "border-radius" "8px", A.style "padding" "12px", A.style "margin" "12px 0" ]
        [ div [ A.style "display" "flex", A.style "align-items" "center", A.style "gap" "16px" ]
            [ div [ A.style "font-size" "20px", A.style "font-weight" "700", A.style "color" "#d97706" ]
                [ text paramName ]
            , div [ A.style "font-size" "28px", A.style "font-weight" "700", A.style "color" "#dc2626", A.style "background" "white", A.style "padding" "4px 12px", A.style "border-radius" "6px" ]
                [ text currentValue ]
            , div [ A.style "flex" "1", A.style "font-size" "14px", A.style "color" "#92400e" ]
                [ text watchFor ]
            ]
        ]


storyBar : Model -> Html Msg
storyBar m =
    let
        storyButton label mode =
            let
                isActive =
                    m.activeStory == Just mode
                
                ( bgColor, borderColor, textColor ) =
                    if isActive then
                        ( "#3b82f6", "#3b82f6", "white" )
                    else
                        ( "white", "#e5e7eb", "#333" )
            in
            button
                [ E.onClick
                    (if isActive then
                        StepStory
                    else
                        StartStory mode
                    )
                , A.style "border" ("2px solid " ++ borderColor)
                , A.style "border-radius" "8px"
                , A.style "padding" "12px 20px"
                , A.style "background" bgColor
                , A.style "color" textColor
                , A.style "font-weight" "600"
                , A.style "cursor" "pointer"
                , A.style "transition" "all 0.2s"
                ]
                [ if isActive then
                    div []
                        [ div [ A.style "font-size" "16px" ] [ text label ]
                        , div [ A.style "font-size" "12px", A.style "margin-top" "4px" ] 
                            [ text ("Step " ++ String.fromInt (m.storyStep + 1) ++ "/4 - Click to continue →") ]
                        ]
                  else
                    text label
                ]
    in
    div [ A.style "margin" "16px 0" ]
        [ div [ A.style "margin-bottom" "8px", A.style "font-weight" "600", A.style "color" "#4b5563" ]
            [ text "Interactive Scenarios: Watch the squeeze happen step-by-step" ]
        , div [ A.style "margin-bottom" "12px", A.style "font-size" "14px", A.style "color" "#6b7280" ]
            [ text "Each scenario shows how different market forces squeeze app profits. Click to start, then advance through 4 steps." ]
        , row
            [ div [ A.style "flex" "1" ]
                [ storyButton "AI Model Evolution" ModelEvolution
                , div [ A.style "font-size" "12px", A.style "color" "#6b7280", A.style "margin-top" "4px" ]
                    [ text "AI models get smarter (K: 2→8)" ]
                ]
            , div [ A.style "flex" "1" ]
                [ storyButton "Price War (p: $0.01→$0.0005)" PriceWar
                , div [ A.style "font-size" "12px", A.style "color" "#6b7280", A.style "margin-top" "4px" ]
                    [ text "Token prices collapse as competition heats up" ]
                ]
            , div [ A.style "flex" "1" ]
                [ storyButton "Moat Erosion (τ: $8k→$500)" MoatErosion
                , div [ A.style "font-size" "12px", A.style "color" "#6b7280", A.style "margin-top" "4px" ]
                    [ text "Integration costs gradually fall" ]
                ]
            , if m.activeStory /= Nothing then
                button
                    [ E.onClick StopStory
                    , A.style "border" "2px solid #ef4444"
                    , A.style "border-radius" "8px"
                    , A.style "padding" "12px 20px"
                    , A.style "background" "white"
                    , A.style "color" "#ef4444"
                    , A.style "font-weight" "600"
                    , A.style "cursor" "pointer"
                    , A.style "margin-left" "20px"
                    ]
                    [ text "Stop Story" ]
              else
                text ""
            ]
        ]


controls : Model -> Html Msg
controls m =
    div [ A.style "display" "grid", A.style "grid-template-columns" "repeat(2,minmax(280px,1fr))", A.style "gap" "12px", A.style "margin" "8px 0 16px 0" ]
        [ group "Labs / Capability"
            [ slider "p  (price per raw token, $)" 0.0001 0.02 0.0001 m.p (\s -> SetFloat .p (\v md -> { md | p = v }) s)
            , slider "K  (capability index)" 0 10 0.1 m.k (\s -> SetFloat .k (\v md -> { md | k = v }) s)
            , slider "σ_base  (floor: intelligence per token at K=0)" 0.5 5 0.1 m.sigmaBase (\s -> SetFloat .sigmaBase (\v md -> { md | sigmaBase = v }) s)
            , slider "σ_gain  (ceiling - floor: max improvement possible)" 0 20 0.5 m.sigmaGain (\s -> SetFloat .sigmaGain (\v md -> { md | sigmaGain = v }) s)
            , slider "σ_slope  (how quickly K improves intelligence)" 0.05 1.0 0.01 m.sigmaSlope (\s -> SetFloat .sigmaSlope (\v md -> { md | sigmaSlope = v }) s)
            ]
        , group "Value & Frictions"
            [ slider "τ  (one-time integration cost, $)" 0 40000 100 m.tau (\s -> SetFloat .tau (\v md -> { md | tau = v }) s)
            , slider "V scale  (max $ value to end-user)" 0 200 1 m.vScale (\s -> SetFloat .vScale (\v md -> { md | vScale = v }) s)
            , slider "V decay  (how fast value diminishes)" 0.0001 0.01 0.0001 m.vDecay (\s -> SetFloat .vDecay (\v md -> { md | vDecay = v }) s)
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

        h2 =
            200  -- height for profit chart

        padL =
            60

        padR =
            20

        padT =
            20

        padB =
            34

        -- Dynamic jMax: calculate based on J* values to ensure they're visible
        jMax =
            let
                -- Get J* values
                jApp = jStarApp m
                jDir = jStarDirect m
                maxJ = max jApp jDir
                -- Always show at least to the larger J*, plus 10% padding
                -- This ensures markers are always visible
                withPadding = maxJ * 1.1
            in
            clampPos withPadding |> min m.jCap

        -- sample curve to determine y-max for scaling
        js =
            sampleRange 0 jMax 256

        rs =
            List.map (r m) js

        pis =
            List.map (piFun m) js

        costs =
            List.map (\j -> pEff m * j) js

        profits =
            List.map (profitAt m) js

        yMaxRaw =
            List.maximum (rs ++ pis ++ costs) |> Maybe.withDefault 1

        yMax =
            if yMaxRaw <= 0 then
                1

            else
                yMaxRaw

        -- For profit chart Y-axis scaling
        profitMaxRaw =
            List.maximum (List.map Basics.abs profits) |> Maybe.withDefault 1
        
        profitMax =
            if profitMaxRaw <= 0 then
                1
            else
                profitMaxRaw * 1.2  -- Add some padding

        toX j =
            let
                x0 =
                    toFloat padL

                x1 =
                    toFloat w - toFloat padR
            in
            x0 + toFloat j / jMax * (x1 - x0)
        
        toXFloat jf =
            let
                x0 =
                    toFloat padL

                x1 =
                    toFloat w - toFloat padR
            in
            x0 + jf / jMax * (x1 - x0)

        toY y =
            let
                y0 =
                    toFloat padT

                y1 =
                    toFloat h - toFloat padB
            in
            -- flip Y for SVG
            y1 - (y / yMax) * (y1 - y0)

        toYProfit y =
            let
                y0 =
                    toFloat padT

                y1 =
                    toFloat h2 - toFloat padB
            in
            -- Center the zero line and flip Y for SVG
            (y0 + y1) / 2 - (y / profitMax) * ((y1 - y0) / 2)

        pathOf : (Basics.Float -> Basics.Float) -> String.String
        pathOf f =
            pathFromPoints (List.map (\j -> ( toX j, toY (f <| toFloat j) )) (List.map (\f_ -> round f_) js))

        jA =
            jStarApp m

        jD =
            jStarDirect m

        marker x lbl color =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (sf x)
                    , SA.x2 (sf x)
                    , SA.y1 (sf (toFloat padT))
                    , SA.y2 (sf (toFloat h - padB))
                    , SA.stroke color
                    , SA.strokeWidth "3"
                    , SA.strokeDasharray "4 4"
                    ]
                    []
                , Svg.text_
                    [ SA.x (sf (x + 6)), SA.y (sf (toFloat padT + 14)), SA.fill color, SA.fontSize "14px", SA.fontWeight "600" ]
                    [ Svg.text lbl ]
                ]

        markerProfit x lbl =
            Svg.g []
                [ Svg.line
                    [ SA.x1 (sf x)
                    , SA.x2 (sf x)
                    , SA.y1 (sf (toFloat padT))
                    , SA.y2 (sf (toFloat h2 - padB))
                    , SA.stroke "#0074d9"
                    , SA.strokeWidth "2"
                    , SA.strokeDasharray "4 4"
                    ]
                    []
                ]

        xAxisTicks =
            ticks 5 jMax

        yAxisTicks =
            ticks 5 yMax
    in
    div [ A.style "margin-top" "10px" ]
        [ h3 [] [ text "Curves" ]
        , div [ A.style "display" "flex", A.style "gap" "20px" ]
            [ div [ A.style "flex" "1" ]
                [ div [ A.style "font-size" "14px", A.style "color" "#6b7280", A.style "margin" "0 0 12px 0", A.style "line-height" "1.5" ]
                    [ text "The charts show app revenue and profit across different intelligence levels. See the blue box for detailed definitions." ]
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
                   , marker (toX <| round jA) ("J* app = " ++ formatK jA) "#0074d9"
                   , marker (toX <| round jD) ("J* direct = " ++ formatK jD) "#f97316"
                   ]
                ++ (if jD / max 0.001 jA > 2 then
                        let
                            xA =
                                toX (round jA)
                            
                            xD =
                                toX (round jD)
                            
                            -- Constrain xD to chart bounds
                            xDClamped =
                                min xD (toFloat w - toFloat padR - 20)
                            
                            bracketY =
                                toFloat h / 2  -- Place in middle of chart
                            
                            showBracket =
                                xDClamped - xA > 20  -- Only show if there's enough space
                        in
                        if showBracket then
                            [ -- Connecting bracket
                              Svg.path
                                [ SA.d ("M " ++ sf xA ++ " " ++ sf (bracketY - 5)
                                        ++ " L " ++ sf xA ++ " " ++ sf (bracketY + 5)
                                        ++ " L " ++ sf xDClamped ++ " " ++ sf (bracketY + 5)
                                        ++ " L " ++ sf xDClamped ++ " " ++ sf (bracketY - 5))
                                , SA.fill "none"
                                , SA.stroke "#dc2626"
                                , SA.strokeWidth "2"
                                ]
                                []
                            , Svg.text_
                                [ SA.x (sf ((xA + xDClamped) / 2))
                                , SA.y (sf (bracketY - 10))
                                , SA.textAnchor "middle"
                                , SA.fill "#dc2626"
                                , SA.fontSize "14px"
                                , SA.fontWeight "600"
                                ]
                                [ Svg.text ("Gap: " ++ fixed 1 (jD / max 0.001 jA) ++ "x") ]
                            ]
                        else
                            []
                    else
                        []
                   )
                ++ [ legend (padL + 600) (padT + 8)
                   ]
            )
                
                    -- Profit Visualization Strip
                    , h3 [ A.style "margin-top" "20px" ] [ text "App Profit" ]
                    , div [ A.style "font-size" "13px", A.style "color" "#6b7280", A.style "margin" "4px 0 8px 0" ]
                        [ text "Shows app's profit for different amounts of intelligence units sold (J): Revenue - Cost. "
                        , span [ A.style "color" "#10b981", A.style "font-weight" "600" ] [ text "Green" ]
                        , text " = profitable. J* marks the peak profit point."
                        ]
                    , profitChart m w h2 padL padR padT padB jMax toXFloat toYProfit profitMax js jA
                    ]
            , div [ A.style "flex" "0 0 300px", A.style "padding-left" "20px" ]
                [ sigmaExplanationCompact m ]
            ]
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
    sliderWithTooltip label_ "" minV maxV stepV val mkMsg


sliderWithTooltip : String -> String -> Float -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
sliderWithTooltip label_ tooltip minV maxV stepV val mkMsg =
    div [ A.style "display" "grid", A.style "grid-template-columns" "1fr auto", A.style "align-items" "center", A.style "gap" "8px", A.style "margin" "6px 0" ]
        [ div [ A.style "position" "relative" ]
            [ label [] [ text (label_ ++ "  ") ]
            , span [ A.style "font-variant-numeric" "tabular-nums", A.style "opacity" "0.8" ] [ text (prettyVal val) ]
            , if tooltip /= "" then
                div [ A.style "margin-top" "4px", A.style "font-size" "12px", A.style "color" "#6b7280", A.style "line-height" "1.4" ]
                    [ text tooltip ]
              else
                text ""
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
        , A.style "border" "2px solid #e5e7eb"
        , A.style "border-radius" "999px"
        , A.style "padding" "10px 16px"
        , A.style "background" "white"
        , A.style "font-weight" "600"
        , A.style "cursor" "pointer"
        , A.style "transition" "all 0.2s"
        , A.style "font-size" "14px"
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


profitChart : Model -> Int -> Int -> Int -> Int -> Int -> Int -> Float -> (Float -> Float) -> (Float -> Float) -> Float -> List Float -> Float -> Html msg
profitChart m w h padL padR padT padB jMax toX toYProfit profitMax js jA =
    let
        -- Create filled areas for positive and negative profit
        profitPoints =
            List.map (\j -> ( toX j, toYProfit (profitAt m j) )) js
        
        zeroY =
            toYProfit 0
        
        -- Split points into positive and negative segments
        createArea points fillColor =
            case points of
                [] ->
                    []
                
                first :: _ ->
                    let
                        pathData =
                            "M " ++ sf (Tuple.first first) ++ " " ++ sf zeroY
                                ++ concatMap (\( x, y ) -> " L " ++ sf x ++ " " ++ sf y) points
                                ++ " L " ++ sf (Tuple.first (List.reverse points |> List.head |> Maybe.withDefault first)) ++ " " ++ sf zeroY
                                ++ " Z"
                    in
                    [ Svg.path
                        [ SA.d pathData
                        , SA.fill fillColor
                        , SA.opacity "0.7"
                        ]
                        []
                    ]
        
        -- Find where profit crosses zero to split areas
        profitAreas =
            let
                profitVals =
                    List.map2 (\j ppt -> ( j, profitAt m j, ppt )) js profitPoints
                
                positiveSegs =
                    profitVals
                        |> List.filter (\( _, profit, _ ) -> profit >= 0)
                        |> List.map (\( _, _, ppt ) -> ppt)
                
                negativeSegs =
                    profitVals
                        |> List.filter (\( _, profit, _ ) -> profit < 0)
                        |> List.map (\( _, _, ppt ) -> ppt)
            in
            createArea positiveSegs "#22c55e" ++ createArea negativeSegs "#ef4444"
        
        -- Y-axis ticks for profit
        profitTicks =
            if profitMax > 100 then
                [ -profitMax * 0.8, -profitMax * 0.4, 0, profitMax * 0.4, profitMax * 0.8 ]
            else
                [ -profitMax * 0.5, 0, profitMax * 0.5 ]
        
        xAxisTicks =
            ticks 5 jMax
    in
    Svg.svg 
        [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
        , SA.width "100%"
        , SA.height (String.fromInt h ++ "px")
        , SA.style "background:#fff;border:1px solid #e5e7eb;border-radius:10px;margin-top:10px"
        ]
        ([ -- axes
           Svg.line [ SA.x1 (sf (toFloat padL)), SA.y1 (sf zeroY), SA.x2 (sf (toFloat (w - padR))), SA.y2 (sf zeroY), SA.stroke "#888", SA.strokeWidth "1" ] []
         , Svg.line [ SA.x1 (sf (toFloat padL)), SA.y1 (sf (toFloat padT)), SA.x2 (sf (toFloat padL)), SA.y2 (sf (toFloat (h - padB))), SA.stroke "#888" ] []
         ]
         ++ profitAreas
         ++ [ -- Profit curve line
              Svg.path
                [ SA.d (pathFromPoints profitPoints)
                , SA.fill "none"
                , SA.stroke "#333"
                , SA.strokeWidth "2"
                ]
                []
            ]
         ++ List.map
            (\t ->
                let
                    x =
                        toX t
                in
                Svg.g []
                    [ Svg.line [ SA.x1 (sf x), SA.x2 (sf x), SA.y1 (sf (toFloat (h - padB))), SA.y2 (sf (toFloat (h - padB + 6))), SA.stroke "#888" ] []
                    , Svg.text_ [ SA.x (sf x), SA.y (sf (toFloat (h - padB + 20))), SA.textAnchor "middle", SA.fill "#666", SA.fontSize "11px" ] [ Svg.text (formatK t) ]
                    ]
            )
            xAxisTicks
         ++ List.map
            (\t ->
                let
                    y =
                        toYProfit t
                in
                Svg.g []
                    [ Svg.line [ SA.x1 (sf (toFloat (padL - 6))), SA.x2 (sf (toFloat padL)), SA.y1 (sf y), SA.y2 (sf y), SA.stroke "#888" ] []
                    , Svg.text_ [ SA.x (sf (toFloat (padL - 10))), SA.y (sf (y + 4)), SA.textAnchor "end", SA.fill "#666", SA.fontSize "11px" ] [ Svg.text ("$" ++ formatK t) ]
                    ]
            )
            profitTicks
         ++ [ -- J* marker
              Svg.line
                [ SA.x1 (sf (toX jA))
                , SA.x2 (sf (toX jA))
                , SA.y1 (sf (toFloat padT))
                , SA.y2 (sf (toFloat (h - padB)))
                , SA.stroke "#0074d9"
                , SA.strokeWidth "3"
                , SA.strokeDasharray "4 4"
                ]
                []
            , Svg.text_
                [ SA.x (sf (toX jA + 6))
                , SA.y (sf (toFloat (padT + 14)))
                , SA.fill "#0074d9"
                , SA.fontSize "14px"
                , SA.fontWeight "600"
                ]
                [ Svg.text "J* (peak)" ]
            -- Add formula label in top-right corner
            , Svg.text_
                [ SA.x (sf (toFloat (w - padR - 10)))
                , SA.y (sf (toFloat (padT + 20)))
                , SA.textAnchor "end"
                , SA.fill "#6b7280"
                , SA.fontSize "11px"
                , SA.fontStyle "italic"
                ]
                [ Svg.text "π(J) = R(J) - (p/σ)·J" ]
            ]
        )


intelligenceGapExplanation : Html msg
intelligenceGapExplanation =
    div [ A.style "margin" "0 auto 20px", A.style "max-width" "600px", A.style "font-size" "14px", A.style "color" "#4b5563", A.style "text-align" "center" ]
        [ p [] 
            [ strong_ "Intelligence Gap = J*_direct / J*_app", 
              text " — The ratio of how much AI intelligence users would buy directly vs through the app."
            ]
        , p [ A.style "margin-top" "8px" ] 
            [ text "Apps choose J* to maximize revenue R(J) - cost, while users choose J* to maximize value Π(J) - cost. "
            , text "The gap grows when apps can't capture the full value they create."
            ]
        ]





-- small helper for bold inline labels


strong_ : String -> Html msg
strong_ s =
    span [ A.style "font-weight" "600" ] [ text s ]


sigmaExplanation : Model -> Html Msg
sigmaExplanation m =
    let
        currentSigma =
            sigma m
    in
    div [ A.style "margin" "16px 0", A.style "padding" "12px", A.style "background" "#e0f2fe", A.style "border-radius" "8px", A.style "font-size" "14px" ]
        [ strong_ ("σ(K) = " ++ fixed 2 currentSigma ++ " intelligence units per token")
        , text " — Think of this as 'how much work each token can do'"
        , ul [ A.style "margin" "8px 0 0 16px", A.style "font-size" "13px" ]
            [ li [] [ text "Intelligence formula: σ(K) = σ_base + σ_gain × (1 - e^(-σ_slope × K))" ]
            , li [] [ text "At K=0 (worst model): σ = ", text (fixed 2 m.sigmaBase), text " (just the floor)" ]
            , li [] [ text "At K=∞ (best possible): σ = ", text (fixed 2 (m.sigmaBase + m.sigmaGain)), text " (floor + ceiling)" ]
            , li [] [ text "Current effective price: p/σ(K) = $", text (fixed 6 (pEff m)), text " per intelligence unit" ]
            ]
        ]


disintermediationDecision : Model -> Html Msg
disintermediationDecision m =
    let
        jA = jStarApp m
        jD = jStarDirect m
        
        -- Via App calculations
        pApp = chosenPapp m jA
        costViaApp = pApp * jA
        valueViaApp = piFun m jA + vFun m jA
        profitViaApp = valueViaApp - costViaApp
        
        -- Direct calculations
        costDirect = pEff m * jD + m.tau
        valueDirect = piFun m jD
        profitDirect = valueDirect - costDirect
        
        -- Which is better?
        directIsBetter = profitDirect > profitViaApp + 1.0e-6
        
        optionStyle selected =
            A.style "flex" "1" ::
            A.style "padding" "16px" ::
            A.style "border-radius" "8px" ::
            A.style "border" (if selected then "3px solid #3b82f6" else "2px solid #e5e7eb") ::
            A.style "background" (if selected then "#eff6ff" else "white") ::
            []
            
        valueRow label value color =
            div [ A.style "display" "flex", A.style "justify-content" "space-between", A.style "margin" "4px 0", A.style "font-size" "14px" ]
                [ span [ A.style "color" "#6b7280" ] [ text label ]
                , span [ A.style "font-weight" "600", A.style "color" color ] [ text value ]
                ]
    in
    div [ A.style "margin" "20px 0" ]
        [ h3 [ A.style "margin" "0 0 12px 0", A.style "font-size" "18px" ] [ text "End-User's Choice: App or Direct?" ]
        , p [ A.style "margin" "0 0 12px 0", A.style "font-size" "13px", A.style "color" "#6b7280" ] 
            [ text "Comparing user's best outcome with each option (at their respective optimal J* values)" ]
        , div [ A.style "display" "flex", A.style "gap" "16px" ]
            [ div (optionStyle (not directIsBetter))
                [ h4 [ A.style "margin" "0 0 12px 0", A.style "color" "#1e40af" ] [ text "Via App (at J* app)" ]
                , valueRow "Intelligence units:" (formatK jA) "#333"
                , valueRow "Pay to app:" ("$" ++ fixed 2 costViaApp) "#dc2626"
                , valueRow "Value received:" ("$" ++ fixed 2 valueViaApp) "#059669"
                , div [ A.style "border-top" "1px solid #e5e7eb", A.style "margin" "8px 0" ] []
                , valueRow "Net benefit:" ("$" ++ fixed 2 profitViaApp) (if directIsBetter then "#6b7280" else "#059669")
                ]
            , div (optionStyle directIsBetter)
                [ h4 [ A.style "margin" "0 0 12px 0", A.style "color" "#ea580c" ] [ text "Go Direct (at J* direct)" ]
                , valueRow "Intelligence units:" (formatK jD) "#333"
                , valueRow "Pay to lab:" ("$" ++ fixed 2 (pEff m * jD)) "#dc2626"
                , valueRow "One-time setup (τ):" ("$" ++ fixed 2 m.tau) "#dc2626"
                , valueRow "Value received:" ("$" ++ fixed 2 valueDirect) "#059669"
                , div [ A.style "border-top" "1px solid #e5e7eb", A.style "margin" "8px 0" ] []
                , valueRow "Net benefit:" ("$" ++ fixed 2 profitDirect) (if directIsBetter then "#059669" else "#6b7280")
                ]
            ]
        , if directIsBetter then
            p [ A.style "text-align" "center", A.style "margin-top" "12px", A.style "font-size" "14px", A.style "color" "#dc2626", A.style "font-weight" "600" ]
                [ text ("Going direct is $" ++ fixed 2 (profitDirect - profitViaApp) ++ " better — the app is squeezed out!") ]
          else
            p [ A.style "text-align" "center", A.style "margin-top" "12px", A.style "font-size" "14px", A.style "color" "#059669" ]
                [ text ("Using the app is $" ++ fixed 2 (profitViaApp - profitDirect) ++ " better — the app survives.") ]
        ]


sigmaExplanationCompact : Model -> Html Msg
sigmaExplanationCompact m =
    let
        currentSigma =
            sigma m
        currentPrice = 
            pEff m
    in
    div [ A.style "background" "#e0f2fe", A.style "border-radius" "8px", A.style "padding" "16px" ]
        [ h4 [ A.style "margin" "0 0 12px 0", A.style "font-size" "16px" ] [ text "Intelligence Multiplier" ]
        , div [ A.style "font-size" "24px", A.style "font-weight" "700", A.style "margin-bottom" "8px" ]
            [ text ("σ(K) = " ++ fixed 2 currentSigma) ]
        , div [ A.style "font-size" "14px", A.style "color" "#1e40af", A.style "margin-bottom" "16px" ]
            [ text "intelligence units per token" ]
        
        , div [ A.style "font-size" "13px", A.style "line-height" "1.6" ]
            [ p [ A.style "margin" "0 0 8px 0" ]
                [ text "Higher σ means smarter models that deliver more intelligence per token." ]
            , p [ A.style "margin" "0 0 12px 0" ]
                [ text ("Formula: σ(K) = " ++ fixed 1 m.sigmaBase ++ " + " ++ fixed 1 m.sigmaGain ++ " × (1 - e^(-" ++ fixed 2 m.sigmaSlope ++ "×K))") ]
            
            , div [ A.style "background" "white", A.style "border-radius" "6px", A.style "padding" "12px", A.style "margin-bottom" "12px" ]
                [ div [ A.style "font-size" "18px", A.style "font-weight" "600", A.style "color" "#1e40af" ]
                    [ text ("$" ++ fixed 6 currentPrice) ]
                , div [ A.style "font-size" "12px", A.style "color" "#64748b" ]
                    [ text "Effective price per intelligence unit" ]
                , div [ A.style "font-size" "11px", A.style "color" "#94a3b8", A.style "margin-top" "4px" ]
                    [ text "= p / σ(K)" ]
                ]
            
            , p [ A.style "margin" "0 0 12px 0", A.style "font-size" "12px", A.style "color" "#64748b" ]
                [ text "As K increases, σ grows and effective price falls, creating the squeeze." ]
            
            , div [ A.style "border-top" "1px solid #cbd5e1", A.style "padding-top" "12px", A.style "margin-top" "12px" ]
                [ div [ A.style "margin" "0 0 8px 0", A.style "font-size" "14px", A.style "font-weight" "600" ] [ text "Chart Definitions" ]
                , p [ A.style "margin" "0 0 6px 0", A.style "font-size" "12px", A.style "line-height" "1.5" ]
                    [ strong "J axis:", text " Effective intelligence units delivered (not raw tokens). J = σ(K) × I, where I = raw tokens" ]
                , p [ A.style "margin" "0 0 4px 0", A.style "font-size" "12px", A.style "line-height" "1.5" ]
                    [ span [ A.style "color" "#2563eb", A.style "font-weight" "600" ] [ text "J* app (blue):" ], text " Optimal point for app, found where the slope of R(J) equals the cost line slope" ]
                , p [ A.style "margin" "0 0 8px 0", A.style "font-size" "12px", A.style "line-height" "1.5" ]
                    [ span [ A.style "color" "#ea580c", A.style "font-weight" "600" ] [ text "J* direct (orange):" ], text " Optimal point for user going direct, where the slope of Π(J) equals the cost line slope" ]
                , div [ A.style "background" "white", A.style "border-radius" "4px", A.style "padding" "8px", A.style "margin-top" "8px", A.style "font-size" "11px", A.style "color" "#64748b" ]
                    [ text "💡 These points maximize profit: where marginal benefit = marginal cost (slope matching)" ]
                ]
            ]
        ]


squeezeMeter : Model -> Html Msg
squeezeMeter m =
    let
        jA =
            jStarApp m

        jD =
            jStarDirect m
        
        -- Intelligence gap ratio
        ratio =
            if jA > 0 then
                jD / jA
            else
                999.0
        
        -- Calculate margin change
        currentMargin =
            appProfitAt m jA / max 0.0001 jA
        
        -- Color thresholds are illustrative, not derived from theory
        ( bgColor, textColor ) =
            if ratio < 2 then
                ( "#22c55e", "white" )  -- Green: App captures decent value
            else if ratio < 5 then
                ( "#eab308", "white" )  -- Yellow: Significant underserving
            else if ratio < 10 then
                ( "#f97316", "white" )  -- Orange: Major misalignment
            else
                ( "#ef4444", "white" )  -- Red: Extreme squeeze
        
        willDisintermediate =
            euProfitDirectAt m jD > euProfitViaAppAt m jA + 1.0e-6
    in
    div [ A.style "margin" "20px 0" ]
        [ row
            [ div [ A.style "background" bgColor, A.style "color" textColor, A.style "padding" "16px 24px", A.style "border-radius" "12px", A.style "flex" "1" ]
                [ div [ A.style "font-size" "24px", A.style "font-weight" "700" ]
                    [ text ("Intelligence Gap: " ++ fixed 2 ratio ++ "x") ]
                , div [ A.style "font-size" "13px", A.style "opacity" "0.9", A.style "margin-top" "4px" ]
                    [ strong "App: ", text ("Stops selling at " ++ formatK jA ++ " units") ]
                , div [ A.style "font-size" "12px", A.style "opacity" "0.8", A.style "margin-left" "8px" ]
                    [ text "→ More would reduce their profit" ]
                , div [ A.style "font-size" "13px", A.style "opacity" "0.9", A.style "margin-top" "4px" ]
                    [ strong "User: ", text ("Would buy up to " ++ formatK jD ++ " units") ]
                , div [ A.style "font-size" "12px", A.style "opacity" "0.8", A.style "margin-left" "8px" ]
                    [ text "→ Value exceeds cost up to this point" ]
                ]
            , div [ A.style "background" "#f3f4f6", A.style "padding" "16px 24px", A.style "border-radius" "12px", A.style "flex" "1" ]
                [ div [ A.style "font-size" "18px", A.style "font-weight" "600" ]
                    [ text ("Margin: $" ++ fixed 4 currentMargin ++ "/unit")
                    , if ratio > 2 then
                        span [ A.style "margin-left" "8px", A.style "color" "#dc2626" ] [ text "↓" ]
                      else
                        text ""
                    ]
                , div [ A.style "font-size" "14px", A.style "color" "#6b7280", A.style "margin-top" "4px" ]
                    [ text "App profit per intelligence unit" ]
                ]
            ]
        ]


squeezeVisualization : Model -> Html Msg
squeezeVisualization m =
    let
        jA = jStarApp m
        jD = jStarDirect m
        
        -- Calculate squeeze metrics
        ratio = if jA > 0 then jD / jA else 999.0
        
        -- Determine if user goes direct
        euViaApp = euProfitViaAppAt m jA
        euDirect = euProfitDirectAt m jD
        willDisintermediate = euDirect > euViaApp + 1.0e-6
        
        -- Visual dimensions
        boxWidth = 600
        boxHeight = 300
        
        -- Create profit comparison bars
        profitMax = max (abs euViaApp) (abs euDirect) * 1.2
        
        -- Colors based on decision
        appColor = if willDisintermediate then "#94a3b8" else "#3b82f6"
        directColor = if willDisintermediate then "#dc2626" else "#94a3b8"
        
        -- Bar heights (normalized to 250px max)
        appBarHeight = if profitMax > 0 then (euViaApp / profitMax * 250) else 0
        directBarHeight = if profitMax > 0 then (euDirect / profitMax * 250) else 0
    in
    div [ A.style "margin" "20px 0", A.style "padding" "20px", A.style "background" "#f9fafb", A.style "border-radius" "12px" ]
        [ h3 [ A.style "margin" "0 0 16px 0", A.style "text-align" "center" ] 
            [ text "Why Users Switch: The Profit Comparison" ]
        
        -- Main profit comparison visualization
        , div [ A.style "display" "flex", A.style "justify-content" "center", A.style "margin-bottom" "20px" ]
            [ Svg.svg 
                [ SA.viewBox ("0 0 " ++ String.fromInt boxWidth ++ " " ++ String.fromInt boxHeight)
                , SA.width (String.fromInt boxWidth)
                , SA.height (String.fromInt boxHeight)
                ]
                [ -- Background
                  Svg.rect
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width (String.fromInt boxWidth)
                    , SA.height (String.fromInt boxHeight)
                    , SA.fill "white"
                    , SA.stroke "#e5e7eb"
                    , SA.strokeWidth "2"
                    , SA.rx "8"
                    ]
                    []
                
                -- Title
                , Svg.text_
                    [ SA.x (String.fromInt (boxWidth // 2))
                    , SA.y "25"
                    , SA.textAnchor "middle"
                    , SA.fontSize "14px"
                    , SA.fontWeight "600"
                    , SA.fill "#374151"
                    ]
                    [ Svg.text "User's Net Profit Comparison" ]
                
                -- Zero line
                , Svg.line
                    [ SA.x1 "50"
                    , SA.x2 (String.fromInt (boxWidth - 50))
                    , SA.y1 "270"
                    , SA.y2 "270"
                    , SA.stroke "#9ca3af"
                    , SA.strokeWidth "1"
                    ]
                    []
                
                -- Via App bar
                , if appBarHeight > 0 then
                    Svg.rect
                        [ SA.x "150"
                        , SA.y (String.fromFloat (270 - appBarHeight))
                        , SA.width "100"
                        , SA.height (String.fromFloat appBarHeight)
                        , SA.fill appColor
                        , SA.rx "4"
                        ]
                        []
                  else
                    Svg.rect
                        [ SA.x "150"
                        , SA.y "270"
                        , SA.width "100"
                        , SA.height (String.fromFloat (abs appBarHeight))
                        , SA.fill "#ef4444"
                        , SA.rx "4"
                        ]
                        []
                
                -- Via App label
                , Svg.text_
                    [ SA.x "200"
                    , SA.y "290"
                    , SA.textAnchor "middle"
                    , SA.fontSize "12px"
                    , SA.fontWeight "600"
                    , SA.fill "#374151"
                    ]
                    [ Svg.text "Via App" ]
                
                -- Via App value
                , Svg.text_
                    [ SA.x "200"
                    , SA.y (if appBarHeight > 20 then String.fromFloat (270 - appBarHeight + 15) else "260")
                    , SA.textAnchor "middle"
                    , SA.fontSize "14px"
                    , SA.fontWeight "700"
                    , SA.fill (if appBarHeight > 20 then "white" else "#374151")
                    ]
                    [ Svg.text ("$" ++ formatK euViaApp) ]
                
                -- Direct bar
                , if directBarHeight > 0 then
                    Svg.rect
                        [ SA.x "350"
                        , SA.y (String.fromFloat (270 - directBarHeight))
                        , SA.width "100"
                        , SA.height (String.fromFloat directBarHeight)
                        , SA.fill directColor
                        , SA.rx "4"
                        ]
                        []
                  else
                    Svg.rect
                        [ SA.x "350"
                        , SA.y "270"
                        , SA.width "100"
                        , SA.height (String.fromFloat (abs directBarHeight))
                        , SA.fill "#ef4444"
                        , SA.rx "4"
                        ]
                        []
                
                -- Direct label
                , Svg.text_
                    [ SA.x "400"
                    , SA.y "290"
                    , SA.textAnchor "middle"
                    , SA.fontSize "12px"
                    , SA.fontWeight "600"
                    , SA.fill "#374151"
                    ]
                    [ Svg.text "Go Direct" ]
                
                -- Direct value
                , Svg.text_
                    [ SA.x "400"
                    , SA.y (if directBarHeight > 20 then String.fromFloat (270 - directBarHeight + 15) else "260")
                    , SA.textAnchor "middle"
                    , SA.fontSize "14px"
                    , SA.fontWeight "700"
                    , SA.fill (if directBarHeight > 20 then "white" else "#374151")
                    ]
                    [ Svg.text ("$" ++ formatK euDirect) ]
                
                -- Winner indicator
                , if willDisintermediate then
                    Svg.text_
                        [ SA.x "400"
                        , SA.y "50"
                        , SA.textAnchor "middle"
                        , SA.fontSize "18px"
                        , SA.fontWeight "700"
                        , SA.fill "#dc2626"
                        ]
                        [ Svg.text "User Goes Direct!" ]
                  else
                    Svg.text_
                        [ SA.x "200"
                        , SA.y "50"
                        , SA.textAnchor "middle"
                        , SA.fontSize "18px"
                        , SA.fontWeight "700"
                        , SA.fill "#3b82f6"
                        ]
                        [ Svg.text "App Survives!" ]
                ]
            ]
        
        -- Explanation text
        , div [ A.style "text-align" "center", A.style "margin-bottom" "16px" ]
            [ if willDisintermediate then
                div [ A.style "font-size" "16px", A.style "color" "#dc2626", A.style "font-weight" "600" ]
                    [ text "❌ USER GOES DIRECT: App is completely bypassed!" ]
              else if ratio > 3 then
                div [ A.style "font-size" "16px", A.style "color" "#f97316", A.style "font-weight" "600" ]
                    [ text ("⚠️ SEVERE UNDERSERVING: App provides only " ++ fixed 0 (100 / ratio) ++ "% of what users want") ]
              else if ratio > 2 then
                div [ A.style "font-size" "16px", A.style "color" "#eab308", A.style "font-weight" "600" ]
                    [ text "⚡ MODERATE GAP: Intelligence mismatch growing" ]
              else
                div [ A.style "font-size" "16px", A.style "color" "#22c55e", A.style "font-weight" "600" ]
                    [ text "✓ WELL ALIGNED: App serves most of user demand" ]
            ]
        
        -- Key insight
        , div [ A.style "background" "white", A.style "border" "1px solid #e5e7eb", A.style "border-radius" "8px", A.style "padding" "16px", A.style "margin-top" "16px" ]
            [ div [ A.style "font-size" "14px", A.style "line-height" "1.6", A.style "color" "#374151" ]
                [ p [ A.style "margin" "0 0 8px 0" ]
                    [ strong "The Squeeze: "
                    , text "Apps maximize profit by stopping at J*app, but users would buy up to J*direct if they could. "
                    , text "This gap represents unserved demand."
                    ]
                , p [ A.style "margin" "0" ]
                    [ strong "Why it matters: "
                    , text "As AI improves (higher K) or integration costs fall (lower τ), this gap grows until users find it profitable to bypass the app entirely."
                    ]
                ]
            ]
        ]
