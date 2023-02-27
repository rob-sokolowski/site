module Gen.Pages exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Effect exposing (Effect)
import ElmSpa.Page
import Gen.Params.Animation
import Gen.Params.BouncingBall
import Gen.Params.ElmUiSvgIssue
import Gen.Params.GameDev
import Gen.Params.Home_
import Gen.Params.Pops
import Gen.Params.ScriptaDemo
import Gen.Params.Snippets
import Gen.Params.SpeedReadDemo
import Gen.Params.VegaLite
import Gen.Params.WordleClone
import Gen.Params.Snippets.AntMarching
import Gen.Params.Snippets.InactionIsAnAction
import Gen.Params.Snippets.RequestResponseModes
import Gen.Params.NotFound
import Gen.Model as Model
import Gen.Msg as Msg
import Gen.Route as Route exposing (Route)
import Page exposing (Page)
import Pages.Animation
import Pages.BouncingBall
import Pages.ElmUiSvgIssue
import Pages.GameDev
import Pages.Home_
import Pages.Pops
import Pages.ScriptaDemo
import Pages.Snippets
import Pages.SpeedReadDemo
import Pages.VegaLite
import Pages.WordleClone
import Pages.Snippets.AntMarching
import Pages.Snippets.InactionIsAnAction
import Pages.Snippets.RequestResponseModes
import Pages.NotFound
import Request exposing (Request)
import Shared
import Task
import Url exposing (Url)
import View exposing (View)


type alias Model =
    Model.Model


type alias Msg =
    Msg.Msg


init : Route -> Shared.Model -> Url -> Key -> ( Model, Effect Msg )
init route =
    case route of
        Route.Animation ->
            pages.animation.init ()
    
        Route.BouncingBall ->
            pages.bouncingBall.init ()
    
        Route.ElmUiSvgIssue ->
            pages.elmUiSvgIssue.init ()
    
        Route.GameDev ->
            pages.gameDev.init ()
    
        Route.Home_ ->
            pages.home_.init ()
    
        Route.Pops ->
            pages.pops.init ()
    
        Route.ScriptaDemo ->
            pages.scriptaDemo.init ()
    
        Route.Snippets ->
            pages.snippets.init ()
    
        Route.SpeedReadDemo ->
            pages.speedReadDemo.init ()
    
        Route.VegaLite ->
            pages.vegaLite.init ()
    
        Route.WordleClone ->
            pages.wordleClone.init ()
    
        Route.Snippets__AntMarching ->
            pages.snippets__antMarching.init ()
    
        Route.Snippets__InactionIsAnAction ->
            pages.snippets__inactionIsAnAction.init ()
    
        Route.Snippets__RequestResponseModes ->
            pages.snippets__requestResponseModes.init ()
    
        Route.NotFound ->
            pages.notFound.init ()


update : Msg -> Model -> Shared.Model -> Url -> Key -> ( Model, Effect Msg )
update msg_ model_ =
    case ( msg_, model_ ) of
        ( Msg.Animation msg, Model.Animation params model ) ->
            pages.animation.update params msg model
    
        ( Msg.BouncingBall msg, Model.BouncingBall params model ) ->
            pages.bouncingBall.update params msg model
    
        ( Msg.ElmUiSvgIssue msg, Model.ElmUiSvgIssue params model ) ->
            pages.elmUiSvgIssue.update params msg model
    
        ( Msg.GameDev msg, Model.GameDev params model ) ->
            pages.gameDev.update params msg model
    
        ( Msg.Pops msg, Model.Pops params model ) ->
            pages.pops.update params msg model
    
        ( Msg.ScriptaDemo msg, Model.ScriptaDemo params model ) ->
            pages.scriptaDemo.update params msg model
    
        ( Msg.Snippets msg, Model.Snippets params model ) ->
            pages.snippets.update params msg model
    
        ( Msg.SpeedReadDemo msg, Model.SpeedReadDemo params model ) ->
            pages.speedReadDemo.update params msg model
    
        ( Msg.VegaLite msg, Model.VegaLite params model ) ->
            pages.vegaLite.update params msg model
    
        ( Msg.WordleClone msg, Model.WordleClone params model ) ->
            pages.wordleClone.update params msg model
    
        ( Msg.Snippets__AntMarching msg, Model.Snippets__AntMarching params model ) ->
            pages.snippets__antMarching.update params msg model
    
        ( Msg.Snippets__InactionIsAnAction msg, Model.Snippets__InactionIsAnAction params model ) ->
            pages.snippets__inactionIsAnAction.update params msg model
    
        ( Msg.Snippets__RequestResponseModes msg, Model.Snippets__RequestResponseModes params model ) ->
            pages.snippets__requestResponseModes.update params msg model

        _ ->
            \_ _ _ -> ( model_, Effect.none )


view : Model -> Shared.Model -> Url -> Key -> View Msg
view model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> View.none
    
        Model.Animation params model ->
            pages.animation.view params model
    
        Model.BouncingBall params model ->
            pages.bouncingBall.view params model
    
        Model.ElmUiSvgIssue params model ->
            pages.elmUiSvgIssue.view params model
    
        Model.GameDev params model ->
            pages.gameDev.view params model
    
        Model.Home_ params ->
            pages.home_.view params ()
    
        Model.Pops params model ->
            pages.pops.view params model
    
        Model.ScriptaDemo params model ->
            pages.scriptaDemo.view params model
    
        Model.Snippets params model ->
            pages.snippets.view params model
    
        Model.SpeedReadDemo params model ->
            pages.speedReadDemo.view params model
    
        Model.VegaLite params model ->
            pages.vegaLite.view params model
    
        Model.WordleClone params model ->
            pages.wordleClone.view params model
    
        Model.Snippets__AntMarching params model ->
            pages.snippets__antMarching.view params model
    
        Model.Snippets__InactionIsAnAction params model ->
            pages.snippets__inactionIsAnAction.view params model
    
        Model.Snippets__RequestResponseModes params model ->
            pages.snippets__requestResponseModes.view params model
    
        Model.NotFound params ->
            pages.notFound.view params ()


subscriptions : Model -> Shared.Model -> Url -> Key -> Sub Msg
subscriptions model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> Sub.none
    
        Model.Animation params model ->
            pages.animation.subscriptions params model
    
        Model.BouncingBall params model ->
            pages.bouncingBall.subscriptions params model
    
        Model.ElmUiSvgIssue params model ->
            pages.elmUiSvgIssue.subscriptions params model
    
        Model.GameDev params model ->
            pages.gameDev.subscriptions params model
    
        Model.Home_ params ->
            pages.home_.subscriptions params ()
    
        Model.Pops params model ->
            pages.pops.subscriptions params model
    
        Model.ScriptaDemo params model ->
            pages.scriptaDemo.subscriptions params model
    
        Model.Snippets params model ->
            pages.snippets.subscriptions params model
    
        Model.SpeedReadDemo params model ->
            pages.speedReadDemo.subscriptions params model
    
        Model.VegaLite params model ->
            pages.vegaLite.subscriptions params model
    
        Model.WordleClone params model ->
            pages.wordleClone.subscriptions params model
    
        Model.Snippets__AntMarching params model ->
            pages.snippets__antMarching.subscriptions params model
    
        Model.Snippets__InactionIsAnAction params model ->
            pages.snippets__inactionIsAnAction.subscriptions params model
    
        Model.Snippets__RequestResponseModes params model ->
            pages.snippets__requestResponseModes.subscriptions params model
    
        Model.NotFound params ->
            pages.notFound.subscriptions params ()



-- INTERNALS


pages :
    { animation : Bundle Gen.Params.Animation.Params Pages.Animation.Model Pages.Animation.Msg
    , bouncingBall : Bundle Gen.Params.BouncingBall.Params Pages.BouncingBall.Model Pages.BouncingBall.Msg
    , elmUiSvgIssue : Bundle Gen.Params.ElmUiSvgIssue.Params Pages.ElmUiSvgIssue.Model Pages.ElmUiSvgIssue.Msg
    , gameDev : Bundle Gen.Params.GameDev.Params Pages.GameDev.Model Pages.GameDev.Msg
    , home_ : Static Gen.Params.Home_.Params
    , pops : Bundle Gen.Params.Pops.Params Pages.Pops.Model Pages.Pops.Msg
    , scriptaDemo : Bundle Gen.Params.ScriptaDemo.Params Pages.ScriptaDemo.Model Pages.ScriptaDemo.Msg
    , snippets : Bundle Gen.Params.Snippets.Params Pages.Snippets.Model Pages.Snippets.Msg
    , speedReadDemo : Bundle Gen.Params.SpeedReadDemo.Params Pages.SpeedReadDemo.Model Pages.SpeedReadDemo.Msg
    , vegaLite : Bundle Gen.Params.VegaLite.Params Pages.VegaLite.Model Pages.VegaLite.Msg
    , wordleClone : Bundle Gen.Params.WordleClone.Params Pages.WordleClone.Model Pages.WordleClone.Msg
    , snippets__antMarching : Bundle Gen.Params.Snippets.AntMarching.Params Pages.Snippets.AntMarching.Model Pages.Snippets.AntMarching.Msg
    , snippets__inactionIsAnAction : Bundle Gen.Params.Snippets.InactionIsAnAction.Params Pages.Snippets.InactionIsAnAction.Model Pages.Snippets.InactionIsAnAction.Msg
    , snippets__requestResponseModes : Bundle Gen.Params.Snippets.RequestResponseModes.Params Pages.Snippets.RequestResponseModes.Model Pages.Snippets.RequestResponseModes.Msg
    , notFound : Static Gen.Params.NotFound.Params
    }
pages =
    { animation = bundle Pages.Animation.page Model.Animation Msg.Animation
    , bouncingBall = bundle Pages.BouncingBall.page Model.BouncingBall Msg.BouncingBall
    , elmUiSvgIssue = bundle Pages.ElmUiSvgIssue.page Model.ElmUiSvgIssue Msg.ElmUiSvgIssue
    , gameDev = bundle Pages.GameDev.page Model.GameDev Msg.GameDev
    , home_ = static Pages.Home_.view Model.Home_
    , pops = bundle Pages.Pops.page Model.Pops Msg.Pops
    , scriptaDemo = bundle Pages.ScriptaDemo.page Model.ScriptaDemo Msg.ScriptaDemo
    , snippets = bundle Pages.Snippets.page Model.Snippets Msg.Snippets
    , speedReadDemo = bundle Pages.SpeedReadDemo.page Model.SpeedReadDemo Msg.SpeedReadDemo
    , vegaLite = bundle Pages.VegaLite.page Model.VegaLite Msg.VegaLite
    , wordleClone = bundle Pages.WordleClone.page Model.WordleClone Msg.WordleClone
    , snippets__antMarching = bundle Pages.Snippets.AntMarching.page Model.Snippets__AntMarching Msg.Snippets__AntMarching
    , snippets__inactionIsAnAction = bundle Pages.Snippets.InactionIsAnAction.page Model.Snippets__InactionIsAnAction Msg.Snippets__InactionIsAnAction
    , snippets__requestResponseModes = bundle Pages.Snippets.RequestResponseModes.page Model.Snippets__RequestResponseModes Msg.Snippets__RequestResponseModes
    , notFound = static Pages.NotFound.view Model.NotFound
    }


type alias Bundle params model msg =
    ElmSpa.Page.Bundle params model msg Shared.Model (Effect Msg) Model Msg (View Msg)


bundle page toModel toMsg =
    ElmSpa.Page.bundle
        { redirecting =
            { model = Model.Redirecting_
            , view = View.none
            }
        , toRoute = Route.fromUrl
        , toUrl = Route.toHref
        , fromCmd = Effect.fromCmd
        , mapEffect = Effect.map toMsg
        , mapView = View.map toMsg
        , toModel = toModel
        , toMsg = toMsg
        , page = page
        }


type alias Static params =
    Bundle params () Never


static : View Never -> (params -> Model) -> Static params
static view_ toModel =
    { init = \params _ _ _ -> ( toModel params, Effect.none )
    , update = \params _ _ _ _ _ -> ( toModel params, Effect.none )
    , view = \_ _ _ _ _ -> View.map never view_
    , subscriptions = \_ _ _ _ _ -> Sub.none
    }
    
