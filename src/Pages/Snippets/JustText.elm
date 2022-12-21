module Pages.Snippets.JustText exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Snippets.JustText exposing (Params)
import Page
import Palette
import Request
import Shared
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


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Snippet - \"Just Text\""
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Source Sans Pro"
                , Font.sansSerif
                ]
            ]
            (viewElements model)
        ]
    }


viewElements : Model -> Element Msg
viewElements model =
    el
        [ width fill
        , height fill
        , padding 10
        ]
        (textColumn
            [ width
                (fill
                    |> maximum 800
                    |> minimum 200
                )
            , height fill
            , centerX
            , Border.color Palette.black
            , Border.width 1
            , spacing 15
            , padding 10
            ]
            (List.map (\p -> paragraph [] [ p ]) paragraphs)
        )


paragraphs =
    [ E.text """Is code "just text"? I say no, and will go further, treating code as just text prioritizes
    individual output over team output. This introduces inter-developer tension** that could likely be
    eradicated by not treating code as such. And, this tension climbs up the chain of command, manifesting as
    a tradeoff between "do it correct and slow, or hack it fast". I feel this is a false dichotomy.

    Better code => better business, the two are aligned. By "better business" I mean more timely,
    and more stable deliverables. However, I think this alignment of goals erodes when engineers treat code
    as "just text".

    What do I mean by "just text"? I'll try to clarify through example;
    Shakespeare is, when reduced, "just letters on a page". Mozart, "just" blobs of ink scattered on some lines.
    But anyone who has read Shakespeare or played Mozart knows those two statements are ridiculous. Code
    is text, absolutely, but it's not "just text". It has semantic meaning. The reader (human or machine) must
    apply external knowledge to the text to understand it, similar to how an
    experienced piano player knows how to interpret blobs of ink.

    So, what is the external information programmers need to understand code? Well, I think this is highly
    dependent on the nature of how the author is treating the code. The spectrum:

    Jinja - I've never seen a jinja-based solution I liked (mustache too). Takes a very literal stance that
    "code is nothing more than 'just text'".

    <insert example.. I need code highlighting />

    JSON (file) -

    YAML - I put yaml one step lower than JSON because it supports comments natively, therefore *some* external
    knowledge can be passed onto the next author.

    DBT


    REST

    JSON (over http)

    graphql

    sql

    lisp (avel)

    http

    And, the BIG one.. git. I feel so torn here. So far I've been arguing that treating code as "just text"
    cost your business money since it increases inter-developer friction (time passes, so on a solo team,
    you today and you 6-months is another form of "inter-developer" friction). However, git is *the thing*
    we all use for collaboration. And, I would never advocate for moving off git until something much better
    comes along.

    Here is a thought I've had, this topic will definitely be something I discuss in future snippets.
    Till then, https://twitter.com/rob_soko/status/1588991988032245761

    ** In the case of a solo developer, remember that time passes. You today may face tension with your code
    from 6 weeks ago.

"""
    ]
