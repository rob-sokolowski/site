module Gen.Pages exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Effect exposing (Effect)
import ElmSpa.Page
import Gen.Params.BouncingBall
import Gen.Params.ElmUiSvgIssue
import Gen.Params.Home_
import Gen.Params.IkedaPattern
import Gen.Params.ParableOfPolygonsClone
import Gen.Params.Pops
import Gen.Params.ScriptaDemo
import Gen.Params.SpeedReadDemo
import Gen.Params.WordleClone
import Gen.Params.Stories.AccelerometerTest
import Gen.Params.Stories.IframeTest
import Gen.Params.Stories.ParableOfPolygonsQa
import Gen.Params.NotFound
import Gen.Model as Model
import Gen.Msg as Msg
import Gen.Route as Route exposing (Route)
import Page exposing (Page)
import Pages.BouncingBall
import Pages.ElmUiSvgIssue
import Pages.Home_
import Pages.IkedaPattern
import Pages.ParableOfPolygonsClone
import Pages.Pops
import Pages.ScriptaDemo
import Pages.SpeedReadDemo
import Pages.WordleClone
import Pages.Stories.AccelerometerTest
import Pages.Stories.IframeTest
import Pages.Stories.ParableOfPolygonsQa
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
        Route.BouncingBall ->
            pages.bouncingBall.init ()
    
        Route.ElmUiSvgIssue ->
            pages.elmUiSvgIssue.init ()
    
        Route.Home_ ->
            pages.home_.init ()
    
        Route.IkedaPattern ->
            pages.ikedaPattern.init ()
    
        Route.ParableOfPolygonsClone ->
            pages.parableOfPolygonsClone.init ()
    
        Route.Pops ->
            pages.pops.init ()
    
        Route.ScriptaDemo ->
            pages.scriptaDemo.init ()
    
        Route.SpeedReadDemo ->
            pages.speedReadDemo.init ()
    
        Route.WordleClone ->
            pages.wordleClone.init ()
    
        Route.Stories__AccelerometerTest ->
            pages.stories__accelerometerTest.init ()
    
        Route.Stories__IframeTest ->
            pages.stories__iframeTest.init ()
    
        Route.Stories__ParableOfPolygonsQa ->
            pages.stories__parableOfPolygonsQa.init ()
    
        Route.NotFound ->
            pages.notFound.init ()


update : Msg -> Model -> Shared.Model -> Url -> Key -> ( Model, Effect Msg )
update msg_ model_ =
    case ( msg_, model_ ) of
        ( Msg.BouncingBall msg, Model.BouncingBall params model ) ->
            pages.bouncingBall.update params msg model
    
        ( Msg.ElmUiSvgIssue msg, Model.ElmUiSvgIssue params model ) ->
            pages.elmUiSvgIssue.update params msg model
    
        ( Msg.IkedaPattern msg, Model.IkedaPattern params model ) ->
            pages.ikedaPattern.update params msg model
    
        ( Msg.ParableOfPolygonsClone msg, Model.ParableOfPolygonsClone params model ) ->
            pages.parableOfPolygonsClone.update params msg model
    
        ( Msg.Pops msg, Model.Pops params model ) ->
            pages.pops.update params msg model
    
        ( Msg.ScriptaDemo msg, Model.ScriptaDemo params model ) ->
            pages.scriptaDemo.update params msg model
    
        ( Msg.SpeedReadDemo msg, Model.SpeedReadDemo params model ) ->
            pages.speedReadDemo.update params msg model
    
        ( Msg.WordleClone msg, Model.WordleClone params model ) ->
            pages.wordleClone.update params msg model
    
        ( Msg.Stories__AccelerometerTest msg, Model.Stories__AccelerometerTest params model ) ->
            pages.stories__accelerometerTest.update params msg model
    
        ( Msg.Stories__IframeTest msg, Model.Stories__IframeTest params model ) ->
            pages.stories__iframeTest.update params msg model
    
        ( Msg.Stories__ParableOfPolygonsQa msg, Model.Stories__ParableOfPolygonsQa params model ) ->
            pages.stories__parableOfPolygonsQa.update params msg model

        _ ->
            \_ _ _ -> ( model_, Effect.none )


view : Model -> Shared.Model -> Url -> Key -> View Msg
view model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> View.none
    
        Model.BouncingBall params model ->
            pages.bouncingBall.view params model
    
        Model.ElmUiSvgIssue params model ->
            pages.elmUiSvgIssue.view params model
    
        Model.Home_ params ->
            pages.home_.view params ()
    
        Model.IkedaPattern params model ->
            pages.ikedaPattern.view params model
    
        Model.ParableOfPolygonsClone params model ->
            pages.parableOfPolygonsClone.view params model
    
        Model.Pops params model ->
            pages.pops.view params model
    
        Model.ScriptaDemo params model ->
            pages.scriptaDemo.view params model
    
        Model.SpeedReadDemo params model ->
            pages.speedReadDemo.view params model
    
        Model.WordleClone params model ->
            pages.wordleClone.view params model
    
        Model.Stories__AccelerometerTest params model ->
            pages.stories__accelerometerTest.view params model
    
        Model.Stories__IframeTest params model ->
            pages.stories__iframeTest.view params model
    
        Model.Stories__ParableOfPolygonsQa params model ->
            pages.stories__parableOfPolygonsQa.view params model
    
        Model.NotFound params ->
            pages.notFound.view params ()


subscriptions : Model -> Shared.Model -> Url -> Key -> Sub Msg
subscriptions model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> Sub.none
    
        Model.BouncingBall params model ->
            pages.bouncingBall.subscriptions params model
    
        Model.ElmUiSvgIssue params model ->
            pages.elmUiSvgIssue.subscriptions params model
    
        Model.Home_ params ->
            pages.home_.subscriptions params ()
    
        Model.IkedaPattern params model ->
            pages.ikedaPattern.subscriptions params model
    
        Model.ParableOfPolygonsClone params model ->
            pages.parableOfPolygonsClone.subscriptions params model
    
        Model.Pops params model ->
            pages.pops.subscriptions params model
    
        Model.ScriptaDemo params model ->
            pages.scriptaDemo.subscriptions params model
    
        Model.SpeedReadDemo params model ->
            pages.speedReadDemo.subscriptions params model
    
        Model.WordleClone params model ->
            pages.wordleClone.subscriptions params model
    
        Model.Stories__AccelerometerTest params model ->
            pages.stories__accelerometerTest.subscriptions params model
    
        Model.Stories__IframeTest params model ->
            pages.stories__iframeTest.subscriptions params model
    
        Model.Stories__ParableOfPolygonsQa params model ->
            pages.stories__parableOfPolygonsQa.subscriptions params model
    
        Model.NotFound params ->
            pages.notFound.subscriptions params ()



-- INTERNALS


pages :
    { bouncingBall : Bundle Gen.Params.BouncingBall.Params Pages.BouncingBall.Model Pages.BouncingBall.Msg
    , elmUiSvgIssue : Bundle Gen.Params.ElmUiSvgIssue.Params Pages.ElmUiSvgIssue.Model Pages.ElmUiSvgIssue.Msg
    , home_ : Static Gen.Params.Home_.Params
    , ikedaPattern : Bundle Gen.Params.IkedaPattern.Params Pages.IkedaPattern.Model Pages.IkedaPattern.Msg
    , parableOfPolygonsClone : Bundle Gen.Params.ParableOfPolygonsClone.Params Pages.ParableOfPolygonsClone.Model Pages.ParableOfPolygonsClone.Msg
    , pops : Bundle Gen.Params.Pops.Params Pages.Pops.Model Pages.Pops.Msg
    , scriptaDemo : Bundle Gen.Params.ScriptaDemo.Params Pages.ScriptaDemo.Model Pages.ScriptaDemo.Msg
    , speedReadDemo : Bundle Gen.Params.SpeedReadDemo.Params Pages.SpeedReadDemo.Model Pages.SpeedReadDemo.Msg
    , wordleClone : Bundle Gen.Params.WordleClone.Params Pages.WordleClone.Model Pages.WordleClone.Msg
    , stories__accelerometerTest : Bundle Gen.Params.Stories.AccelerometerTest.Params Pages.Stories.AccelerometerTest.Model Pages.Stories.AccelerometerTest.Msg
    , stories__iframeTest : Bundle Gen.Params.Stories.IframeTest.Params Pages.Stories.IframeTest.Model Pages.Stories.IframeTest.Msg
    , stories__parableOfPolygonsQa : Bundle Gen.Params.Stories.ParableOfPolygonsQa.Params Pages.Stories.ParableOfPolygonsQa.Model Pages.Stories.ParableOfPolygonsQa.Msg
    , notFound : Static Gen.Params.NotFound.Params
    }
pages =
    { bouncingBall = bundle Pages.BouncingBall.page Model.BouncingBall Msg.BouncingBall
    , elmUiSvgIssue = bundle Pages.ElmUiSvgIssue.page Model.ElmUiSvgIssue Msg.ElmUiSvgIssue
    , home_ = static Pages.Home_.view Model.Home_
    , ikedaPattern = bundle Pages.IkedaPattern.page Model.IkedaPattern Msg.IkedaPattern
    , parableOfPolygonsClone = bundle Pages.ParableOfPolygonsClone.page Model.ParableOfPolygonsClone Msg.ParableOfPolygonsClone
    , pops = bundle Pages.Pops.page Model.Pops Msg.Pops
    , scriptaDemo = bundle Pages.ScriptaDemo.page Model.ScriptaDemo Msg.ScriptaDemo
    , speedReadDemo = bundle Pages.SpeedReadDemo.page Model.SpeedReadDemo Msg.SpeedReadDemo
    , wordleClone = bundle Pages.WordleClone.page Model.WordleClone Msg.WordleClone
    , stories__accelerometerTest = bundle Pages.Stories.AccelerometerTest.page Model.Stories__AccelerometerTest Msg.Stories__AccelerometerTest
    , stories__iframeTest = bundle Pages.Stories.IframeTest.page Model.Stories__IframeTest Msg.Stories__IframeTest
    , stories__parableOfPolygonsQa = bundle Pages.Stories.ParableOfPolygonsQa.page Model.Stories__ParableOfPolygonsQa Msg.Stories__ParableOfPolygonsQa
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
    
