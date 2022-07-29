module Pages.SpeedReadDemo exposing (Model, Msg, page)

import Array as A
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.SpeedReadDemo exposing (Params)
import Page
import Palette
import Request
import Shared
import Time
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


type alias CurrentIndex =
    Int


type alias FrameMs =
    Int


type SpeedReadState
    = Paused CurrentIndex FrameMs
    | Playing CurrentIndex FrameMs


type alias Model =
    { text : A.Array String
    , state : SpeedReadState
    , sliderVal : Float
    }


freshModel : Model
freshModel =
    let
        textList =
            String.split " " greatGatsbyChapter1

        textList2 =
            List.filter (\e -> not <| String.startsWith "\n" e) textList
    in
    { text = A.fromList textList2
    , state = Playing 0 150
    , sliderVal = 0.5
    }


init : ( Model, Effect Msg )
init =
    ( freshModel
    , Effect.none
    )


config =
    { maxWpm = 1500
    , minWpm = 60
    }



-- UPDATE


type Msg
    = StartSpeedReading
    | PauseSpeedReading
    | ChangeFrameMs Float
    | Restart
    | FrameTick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        StartSpeedReading ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Playing ix frameMs

                        Paused ix frameMs ->
                            Playing ix frameMs
            in
            ( { model | state = newState }, Effect.none )

        PauseSpeedReading ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Paused ix frameMs

                        Paused ix frameMs ->
                            Paused ix frameMs
            in
            ( { model | state = newState }, Effect.none )

        ChangeFrameMs pct ->
            let
                newFrameMs =
                    round <| ((config.maxWpm - config.minWpm) * pct) + config.minWpm

                newState =
                    case model.state of
                        Paused ix _ ->
                            Paused ix newFrameMs

                        Playing ix _ ->
                            -- throw away current frameMs, use new
                            Playing ix newFrameMs
            in
            ( { model
                | state = newState
                , sliderVal = pct
              }
            , Effect.none
            )

        FrameTick _ ->
            let
                newState =
                    case model.state of
                        Playing ix frameMs ->
                            Playing (modBy (A.length model.text) (ix + 1)) frameMs

                        Paused ix newFrameMs ->
                            Paused ix newFrameMs
            in
            ( { model | state = newState }, Effect.none )

        Restart ->
            ( freshModel, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        frameMs : Float
        frameMs =
            case model.state of
                Paused _ frameMs_ ->
                    toFloat frameMs_

                Playing _ frameMs_ ->
                    toFloat frameMs_
    in
    case model.state of
        Paused _ _ ->
            Sub.none

        Playing _ _ ->
            Sub.batch
                [ Time.every frameMs FrameTick
                ]



-- VIEW


elements : Model -> Element Msg
elements model =
    let
        controlPanel : Element Msg
        controlPanel =
            let
                ( buttonCmd, buttonLbl ) =
                    case model.state of
                        Paused _ _ ->
                            ( StartSpeedReading, "Play" )

                        Playing _ _ ->
                            ( PauseSpeedReading, "Pause" )

                framesMs =
                    case model.state of
                        Paused _ frameMs ->
                            toFloat frameMs

                        Playing _ frameMs ->
                            toFloat frameMs

                wpm : Int
                wpm =
                    round <| (1000 / framesMs) * 60
            in
            column
                [ width fill
                , height <| px 150
                , Border.color Palette.black
                , Border.width 1
                , centerX
                , alignBottom
                , padding 10
                ]
                [ row
                    [ width fill
                    ]
                    [ el
                        [ centerY
                        , alignLeft
                        ]
                        (text <| "Current speed: " ++ String.fromInt wpm ++ " words per minute")
                    ]
                , Input.slider
                    [ height fill
                    , behindContent <|
                        -- Slider track
                        el
                            [ width fill
                            , height <| px 5
                            , centerY
                            , Background.color Palette.darkishGrey
                            , Border.rounded 6
                            ]
                            E.none
                    ]
                    { onChange = ChangeFrameMs
                    , label =
                        Input.labelBelow [ Font.size 10 ] <|
                            text "Move slider to adjust speed"
                    , min = 0.0
                    , max = 1.0
                    , step = Just 0.01
                    , value = model.sliderVal
                    , thumb =
                        Input.thumb
                            [ width <| px 60
                            , height <| px 60
                            , Border.width 2
                            , Border.rounded 6
                            , Border.color Palette.darkCharcoal
                            , Background.color Palette.white
                            ]
                    }
                , Input.button
                    [ Border.color Palette.lightGrey
                    , Border.width 1
                    , Border.rounded 4
                    , alignRight
                    , width <| px 100
                    , padding 4
                    , Background.color Palette.darkishGrey
                    , Border.color Palette.darkCharcoal
                    , moveUp 15
                    ]
                    { onPress = Just buttonCmd
                    , label = el [ centerX ] <| text buttonLbl
                    }
                ]

        promptPanel : Element Msg
        promptPanel =
            let
                message =
                    case model.state of
                        Playing ix _ ->
                            case A.get ix model.text of
                                Nothing ->
                                    "ERROR!"

                                Just v ->
                                    v

                        Paused ix _ ->
                            case A.get ix model.text of
                                Nothing ->
                                    "ERROR!"

                                Just v ->
                                    v
            in
            el
                [ centerX
                , centerY
                , width fill
                , height fill
                , Border.width 1
                , Border.color Palette.black
                , Background.color Palette.lightGrey
                ]
            <|
                (el [ centerX, centerY, Font.size 28 ] <| text message)
    in
    column
        [ width (fill |> maximum 800)
        , height fill
        , centerX
        , Font.size 14
        ]
        [ promptPanel
        , controlPanel
        ]


view : Model -> View Msg
view model =
    { title = "Speed Read Demo"
    , body =
        [ layout
            [ E.width E.fill
            , E.height E.fill
            , centerX
            , padding 5
            ]
            (elements model)
        ]
    }



-- begin region data


greatGatsbyChapter1 =
    """In my younger and more vulnerable years my father gave me some advice that I've been turning over in my mind ever since.

"Whenever you feel like criticizing any one," he told me, "just remember that all the people in this world haven't had the advantages that you've had."

He didn't say any more but we've always been unusually communicative in a reserved way, and I understood that he meant a great deal more than that. In consequence I'm inclined to reserve all judgments, a habit that has opened up many curious natures to me and also made me the victim of not a few veteran bores. The abnormal mind is quick to detect and attach itself to this quality when it appears in a normal person, and so it came about that in college I was unjustly accused of being a politician, because I was privy to the secret griefs of wild, unknown men. Most of the confidences were unsought—frequently I have feigned sleep, preoccupation, or a hostile levity when I realized by some unmistakable sign that an intimate revelation was quivering on the horizon—for the intimate revelations of young men or at least the terms in which they express them are usually plagiaristic and marred by obvious suppressions. Reserving judgments is a matter of infinite hope. I am still a little afraid of missing something if I forget that, as my father snobbishly suggested, and I snobbishly repeat, a sense of the fundamental decencies is parcelled out unequally at birth.

And, after boasting this way of my tolerance, I come to the admission that it has a limit. Conduct may be founded on the hard rock or the wet marshes but after a certain point I don't care what it's founded on. When I came back from the East last autumn I felt that I wanted the world to be in uniform and at a sort of moral attention forever; I wanted no more riotous excursions with privileged glimpses into the human heart. Only Gatsby, the man who gives his name to this book, was exempt from my reaction—Gatsby who represented everything for which I have an unaffected scorn. If personality is an unbroken series of successful gestures, then there was something gorgeous about him, some heightened sensitivity to the promises of life, as if he were related to one of those intricate machines that register earthquakes ten thousand miles away. This responsiveness had nothing to do with that flabby impressionability which is dignified under the name of the "creative temperament"—it was an extraordinary gift for hope, a romantic readiness such as I have never found in any other person and which it is not likely I shall ever find again. No—Gatsby turned out all right at the end; it is what preyed on Gatsby, what foul dust floated in the wake of his dreams that temporarily closed out my interest in the abortive sorrows and short-winded elations of men.



My family have been prominent, well-to-do people in this middle-western city for three generations. The Carraways are something of a clan and we have a tradition that we're descended from the Dukes of Buccleuch, but the actual founder of my line was my grandfather's brother who came here in fifty-one, sent a substitute to the Civil War and started the wholesale hardware business that my father carries on today.

I never saw this great-uncle but I'm supposed to look like him—with special reference to the rather hard-boiled painting that hangs in Father's office. I graduated from New Haven in 1915, just a quarter of a century after my father, and a little later I participated in that delayed Teutonic migration known as the Great War. I enjoyed the counter-raid so thoroughly that I came back restless. Instead of being the warm center of the world the middle-west now seemed like the ragged edge of the universe—so I decided to go east and learn the bond business. Everybody I knew was in the bond business so I supposed it could support one more single man. All my aunts and uncles talked it over as if they were choosing a prep-school for me and finally said, "Why—ye-es" with very grave, hesitant faces. Father agreed to finance me for a year and after various delays I came east, permanently, I thought, in the spring of twenty-two.

The practical thing was to find rooms in the city but it was a warm season and I had just left a country of wide lawns and friendly trees, so when a young man at the office suggested that we take a house together in a commuting town it sounded like a great idea. He found the house, a weather beaten cardboard bungalow at eighty a month, but at the last minute the firm ordered him to Washington and I went out to the country alone. I had a dog, at least I had him for a few days until he ran away, and an old Dodge and a Finnish woman who made my bed and cooked breakfast and muttered Finnish wisdom to herself over the electric stove.

It was lonely for a day or so until one morning some man, more recently arrived than I, stopped me on the road.

"How do you get to West Egg village?" he asked helplessly.

I told him. And as I walked on I was lonely no longer. I was a guide, a pathfinder, an original settler. He had casually conferred on me the freedom of the neighborhood.

And so with the sunshine and the great bursts of leaves growing on the trees—just as things grow in fast movies—I had that familiar conviction that life was beginning over again with the summer.

There was so much to read for one thing and so much fine health to be pulled down out of the young breath-giving air. I bought a dozen volumes on banking and credit and investment securities and they stood on my shelf in red and gold like new money from the mint, promising to unfold the shining secrets that only Midas and Morgan and Maecenas knew. And I had the high intention of reading many other books besides. I was rather literary in college—one year I wrote a series of very solemn and obvious editorials for the "Yale News"—and now I was going to bring back all such things into my life and become again that most limited of all specialists, the "well-rounded man." This isn't just an epigram—life is much more successfully looked at from a single window, after all.

It was a matter of chance that I should have rented a house in one of the strangest communities in North America. It was on that slender riotous island which extends itself due east of New York and where there are, among other natural curiosities, two unusual formations of land. Twenty miles from the city a pair of enormous eggs, identical in contour and separated only by a courtesy bay, jut out into the most domesticated body of salt water in the Western Hemisphere, the great wet barnyard of Long Island Sound. They are not perfect ovals—like the egg in the Columbus story they are both crushed flat at the contact end—but their physical resemblance must be a source of perpetual confusion to the gulls that fly overhead. To the wingless a more arresting phenomenon is their dissimilarity in every particular except shape and size.

I lived at West Egg, the—well, the less fashionable of the two, though this is a most superficial tag to express the bizarre and not a little sinister contrast between them. My house was at the very tip of the egg, only fifty yards from the Sound, and squeezed between two huge places that rented for twelve or fifteen thousand a season. The one on my right was a colossal affair by any standard—it was a factual imitation of some Hôtel de Ville in Normandy, with a tower on one side, spanking new under a thin beard of raw ivy, and a marble swimming pool and more than forty acres of lawn and garden. It was Gatsby's mansion. Or rather, as I didn't know Mr. Gatsby it was a mansion inhabited by a gentleman of that name. My own house was an eye-sore, but it was a small eye-sore, and it had been overlooked, so I had a view of the water, a partial view of my neighbor's lawn, and the consoling proximity of millionaires—all for eighty dollars a month.

Across the courtesy bay the white palaces of fashionable East Egg glittered along the water, and the history of the summer really begins on the evening I drove over there to have dinner with the Tom Buchanans. Daisy was my second cousin once removed and I'd known Tom in college. And just after the war I spent two days with them in Chicago.

Her husband, among various physical accomplishments, had been one of the most powerful ends that ever played football at New Haven—a national figure in a way, one of those men who reach such an acute limited excellence at twenty-one that everything afterward savors of anti-climax. His family were enormously wealthy—even in college his freedom with money was a matter for reproach—but now he'd left Chicago and come east in a fashion that rather took your breath away: for instance he'd brought down a string of polo ponies from Lake Forest. It was hard to realize that a man in my own generation was wealthy enough to do that.
"""



-- end region data
