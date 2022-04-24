module Pages.Pops exposing (Model, Msg, page)

import Color
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Pops exposing (Params)
import Page
import Request
import Shared
import Time
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type SimStatus
    = Running
    | Paused


type alias Model =
    { simStatus : SimStatus
    , currentTickType : TickType
    , world : World
    , polygons : Dict PolygonId Polygon
    , companies : Dict CompanyId Company
    , currentInspectedEntity : Maybe Entity
    }


type Entity
    = PolygonEntity PolygonId
    | CompanyEntity CompanyId


type alias CompanyId =
    String


type alias Company =
    { id : CompanyId
    , applicantQueue : List Polygon
    }


type alias RunConfig =
    { initialPopulationCount : Int
    , numTicks : Int
    }


type alias Year =
    Float


type alias World =
    { currentYear : Year
    }


type alias PolygonId =
    String


type alias Age =
    Float


type EmploymentStatus
    = Unemployed
    | Employed Company


toEmploymentStr : EmploymentStatus -> String
toEmploymentStr es =
    case es of
        Unemployed ->
            "unemployed"

        Employed company ->
            "employed by: " ++ company.id


type alias Polygon =
    { id : PolygonId
    , gender : Gender
    , employmentStatus : EmploymentStatus
    , color : Color
    , age : Age
    }


type Gender
    = Male
    | Female


toGenderStr : Gender -> String
toGenderStr g =
    case g of
        Male ->
            "male"

        Female ->
            "female"


type Color
    = Purple
    | Blue
    | Green
    | Pink
    | Gray


toColorStr : Color -> String
toColorStr c =
    case c of
        Purple ->
            "purple"

        Blue ->
            "blue"

        Gray ->
            "gray"

        Green ->
            "green"

        Pink ->
            "pink"


resetWorld : World
resetWorld =
    { currentYear = 0.0
    }


resetPolygons : Dict PolygonId Polygon
resetPolygons =
    Dict.fromList
        [ ( "p1", { id = "p1", gender = Male, color = Purple, age = 0.0, employmentStatus = Unemployed } )
        , ( "p2", { id = "p2", gender = Male, color = Blue, age = 0.0, employmentStatus = Unemployed } )
        , ( "p3", { id = "p3", gender = Female, color = Green, age = 0.0, employmentStatus = Unemployed } )
        , ( "p4", { id = "p4", gender = Male, color = Purple, age = 0.0, employmentStatus = Unemployed } )
        , ( "p5", { id = "p5", gender = Female, color = Pink, age = 0.0, employmentStatus = Unemployed } )
        , ( "p6", { id = "p6", gender = Female, color = Blue, age = 0.0, employmentStatus = Unemployed } )
        , ( "p7", { id = "p7", gender = Female, color = Purple, age = 0.0, employmentStatus = Unemployed } )
        , ( "p8", { id = "p8", gender = Male, color = Pink, age = 0.0, employmentStatus = Unemployed } )
        , ( "p9", { id = "p9", gender = Female, color = Gray, age = 0.0, employmentStatus = Unemployed } )
        ]


resetCompanies : Dict CompanyId Company
resetCompanies =
    Dict.fromList [ ( "c1", { id = "c1", applicantQueue = [] } ) ]


init : ( Model, Effect Msg )
init =
    ( { simStatus = Paused
      , currentTickType = WorldUpdates
      , world = resetWorld
      , polygons = resetPolygons
      , companies = resetCompanies
      , currentInspectedEntity = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToggleSimStatus
    | ResetSimulation
    | Tick Time.Posix
    | ClickedInspectPolygon PolygonId


type TickType
    = WorldUpdates
    | PolygonUpdates
    | CompanyUpdates
    | CompaniesProcessApplicantQueues


nextTickType : TickType -> TickType
nextTickType tt =
    case tt of
        WorldUpdates ->
            PolygonUpdates

        PolygonUpdates ->
            CompanyUpdates

        CompanyUpdates ->
            CompaniesProcessApplicantQueues

        CompaniesProcessApplicantQueues ->
            WorldUpdates


dt : Float
dt =
    -- the number of years per tick
    1.0


updatePolygons : Model -> Dict PolygonId Polygon
updatePolygons model =
    let
        polygonsAge : PolygonId -> Polygon -> Polygon
        polygonsAge pid p =
            { p | age = p.age + dt }
    in
    Dict.map polygonsAge model.polygons


processApplicants : Model -> ( Dict CompanyId Company, Dict PolygonId Polygon )
processApplicants model =
    let
        applicantQueues : List (List ( Company, Polygon ))
        applicantQueues =
            List.map (\c -> List.map (\p_ -> ( c, p_ )) c.applicantQueue) (Dict.values model.companies)

        polygonsToEmploy : List ( Company, Polygon )
        polygonsToEmploy =
            List.concat applicantQueues

        employPolygon : ( Company, Polygon ) -> Polygon
        employPolygon ( c, p ) =
            { p | employmentStatus = Employed c }

        processPolygons : List Polygon -> List Polygon
        processPolygons polygons =
            let
                psToEmploy =
                    List.map (\( _, p ) -> p) polygonsToEmploy

                idlePolygons =
                    List.filter (\p -> not <| List.member p psToEmploy) polygons

                newlyEmployedPolygons =
                    List.map employPolygon polygonsToEmploy
            in
            idlePolygons ++ newlyEmployedPolygons

        newPolygons =
            processPolygons (Dict.values model.polygons)

        clearQueue : CompanyId -> Company -> Company
        clearQueue _ c =
            { c | applicantQueue = [] }

        newCompanies =
            Dict.map clearQueue model.companies
    in
    ( newCompanies, Dict.fromList <| List.map (\p -> ( p.id, p )) newPolygons )


updateCompanies : Model -> Dict CompanyId Company
updateCompanies model =
    let
        -- TODO: more than 1 company
        theCompany : Maybe Company
        theCompany =
            Dict.get "c1" model.companies

        newCompanies : Dict CompanyId Company
        newCompanies =
            case theCompany of
                Just company ->
                    let
                        adultPolygonsApplyForWork : Polygon -> Maybe ( Polygon, CompanyId )
                        adultPolygonsApplyForWork p =
                            case p.employmentStatus of
                                Unemployed ->
                                    -- TODO: Must apply to more than 1 company
                                    if p.age >= 18.0 then
                                        Just ( p, company.id )

                                    else
                                        Nothing

                                Employed _ ->
                                    Nothing

                        applicantList : List (Maybe ( Polygon, CompanyId ))
                        applicantList =
                            List.map adultPolygonsApplyForWork (Dict.values model.polygons)

                        updateCompanyQueue : CompanyId -> Company -> Company
                        updateCompanyQueue _ c =
                            let
                                applicantList_ =
                                    removeNothingFromList applicantList

                                companyList : List Polygon
                                companyList =
                                    List.map (\( polygon, _ ) -> polygon) <| List.filter (\( _, cid ) -> c.id == cid) applicantList_
                            in
                            { c | applicantQueue = c.applicantQueue ++ companyList }
                    in
                    Dict.map updateCompanyQueue model.companies

                Nothing ->
                    model.companies
    in
    newCompanies


updateWorld : Model -> World
updateWorld model =
    let
        world =
            model.world
    in
    { world | currentYear = model.world.currentYear + dt }


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleSimStatus ->
            let
                newStatus =
                    case model.simStatus of
                        Paused ->
                            Running

                        Running ->
                            Paused
            in
            ( { model | simStatus = newStatus }, Effect.none )

        Tick _ ->
            let
                ( newWorld, newCompanies, newPolygons ) =
                    case model.currentTickType of
                        WorldUpdates ->
                            ( updateWorld model, model.companies, model.polygons )

                        PolygonUpdates ->
                            ( model.world, model.companies, updatePolygons model )

                        CompanyUpdates ->
                            ( model.world, updateCompanies model, model.polygons )

                        CompaniesProcessApplicantQueues ->
                            let
                                ( newCompanies_, newPolygons_ ) =
                                    processApplicants model
                            in
                            ( model.world, newCompanies_, newPolygons_ )
            in
            ( { model
                | world = newWorld
                , companies = newCompanies
                , polygons = newPolygons
                , currentTickType = nextTickType model.currentTickType
              }
            , Effect.none
            )

        ResetSimulation ->
            let
                newWorld =
                    resetWorld
            in
            ( { model
                | world = newWorld
                , simStatus = Paused
                , polygons = resetPolygons
                , companies = resetCompanies
              }
            , Effect.none
            )

        ClickedInspectPolygon pid ->
            ( { model | currentInspectedEntity = Just (PolygonEntity pid) }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.simStatus of
        Paused ->
            Sub.none

        Running ->
            Time.every 0 Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pops"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            ]
            (elements model)
        ]
    }


table : Model -> Element Msg
table model =
    E.table
        [ E.centerX
        , E.centerY
        , E.padding 10
        ]
        { data = Dict.values model.polygons
        , columns =
            [ { header = E.text "id"
              , width = px 200
              , view =
                    \poly ->
                        el
                            [ Border.color UI.palette.darkCharcoal
                            , Border.width 1
                            , onClick <| ClickedInspectPolygon poly.id
                            ]
                            (E.text poly.id)
              }
            , { header = E.text "gender"
              , width = fill
              , view =
                    \poly ->
                        el
                            [ Border.color UI.palette.darkCharcoal
                            , Border.width 1
                            , onClick <| ClickedInspectPolygon poly.id
                            ]
                            (E.text <| toGenderStr poly.gender)
              }
            , { header = E.text "employment"
              , width = fill
              , view =
                    \poly ->
                        el
                            [ Border.color UI.palette.darkCharcoal
                            , Border.width 1
                            , onClick <| ClickedInspectPolygon poly.id
                            ]
                            (E.text <| toEmploymentStr poly.employmentStatus)
              }
            , { header = E.text "age"
              , width = fill
              , view =
                    \poly ->
                        el
                            [ Border.color UI.palette.darkCharcoal
                            , Border.width 1
                            , onClick <| ClickedInspectPolygon poly.id
                            ]
                            (E.text <| String.fromFloat poly.age)
              }
            , { header = E.text "color"
              , width = fill
              , view =
                    \poly ->
                        el
                            [ Border.color UI.palette.darkCharcoal
                            , Border.width 1
                            , onClick <| ClickedInspectPolygon poly.id
                            ]
                            (E.text <| toColorStr poly.color)
              }
            ]
        }


inspectorPanel : Model -> Element Msg
inspectorPanel model =
    case model.currentInspectedEntity of
        Nothing ->
            E.text "Select an entity to inspect"

        Just v ->
            case v of
                PolygonEntity pid ->
                    E.text <| "You have select polygon " ++ pid

                CompanyEntity cid ->
                    E.text <| "You have select company " ++ cid


content : Model -> Element Msg
content model =
    let
        label =
            case model.simStatus of
                Running ->
                    "Pause Sim."

                Paused ->
                    "Resume Sim."

        playPauseButton =
            Input.button
                [ padding 5
                , alignRight
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                , Background.color UI.palette.lightBlue
                ]
                { onPress = Just ToggleSimStatus
                , label = text label
                }

        resetButton =
            Input.button
                [ padding 5
                , alignRight
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                , Background.color UI.palette.lightBlue
                ]
                { onPress = Just ResetSimulation
                , label = text "Reset."
                }
    in
    column [ centerX ]
        [ row
            [ padding 10
            , spacing 5
            ]
            [ resetButton
            , playPauseButton
            ]
        , el
            [ Background.color UI.palette.white
            , Border.width 2
            , Border.color UI.palette.lightGrey
            ]
          <|
            table model
        , inspectorPanel model
        ]


elements : Model -> Element Msg
elements model =
    let
        header : Element msg
        header =
            row
                [ E.width E.fill
                , padding 10
                , spacing 10
                , Background.color UI.palette.lightBlue
                ]
                [ logo
                , el [ alignRight ] <| text "Header"
                , el [ alignRight ] <| text "Stuff"
                , el [ alignRight ] <| text "Goes"
                , el [ alignRight ] <| text "Here"
                ]

        logo : Element msg
        logo =
            el
                [ E.width <| E.px 80
                , E.height <| E.px 40
                , Border.width 2
                , Border.rounded 6
                , Border.color UI.palette.blue
                ]
                (el
                    [ centerX
                    , centerY
                    ]
                 <|
                    text "LOGO"
                )

        footer : Element msg
        footer =
            row
                [ E.width E.fill
                , padding 5
                , Background.color UI.palette.lightBlue
                , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                , Border.color UI.palette.lightGrey
                ]
                [ row
                    [ alignLeft
                    ]
                    [ el [ alignLeft ] <| text "Footer stuff"
                    ]
                ]
    in
    -- TODO: Do I want a footer? If yes, I think I need to grab browser size / resize events to properly float on bottom
    E.column
        [ E.width E.fill
        , E.height E.fill
        , Background.color UI.palette.lightGrey
        , Font.size 12
        ]
        [ header
        , content model

        --, footer
        ]



-- utils


removeNothingFromList : List (Maybe a) -> List a
removeNothingFromList list =
    List.filterMap identity list
