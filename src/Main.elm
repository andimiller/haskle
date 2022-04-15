module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, padding, rgb255, rgba255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, button, div, input)
import Html.Attributes exposing (disabled, hidden, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import List exposing (map, map2, member)
import Random
import Random.Extra exposing (sample)
import String exposing (toList)
import Task
import Tuple exposing (pair)


type GuessStatus
    = Correct
    | Elsewhere
    | Incorrect
    | Blank


type alias Guess =
    ( Char, GuessStatus )


wordlist =
    [ ( "succ", "a -> a" )
    , ( "id", "a -> a" )
    , ( "seq", "a -> b -> b" )
    , ( "curry", "((a, b) -> c) -> a -> b -> c" )
    , ( "uncurry", "(a -> b -> c) -> (a, b) -> c" )
    ]


pickPuzzle =
    sample wordlist
        |> Random.map (Maybe.withDefault ( "hello", "world" ))


start : () -> ( State, Cmd Msg )
start () =
    ( State "" "" "" [] True, Random.generate Init pickPuzzle )


main =
    Browser.document { init = start, update = update, view = view, subscriptions = subscriptions }


renderTarget : String -> List Guess
renderTarget word =
    map (\i -> ( '#', Blank )) (toList word)


type alias State =
    { word : String, hint : String, input : String, guesses : List (List Guess), won : Bool }


type Msg
    = Reroll
    | Init ( String, String )
    | Guess
    | Update String
    | NoOp


markLetter : List Char -> Char -> Char -> Guess
markLetter ts t g =
    if t == g then
        ( g, Correct )

    else if List.member g ts then
        ( g, Elsewhere )

    else
        ( g, Incorrect )


markInput : String -> String -> List Guess
markInput target guess =
    map2 (markLetter (toList target)) (toList target) (toList guess)


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "guess-box")


correct guess =
    Tuple.second guess == Correct


hasWon : List Guess -> Bool
hasWon =
    List.all correct


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Reroll ->
            start ()

        NoOp ->
            ( state, Cmd.none )

        Init ( target, hint ) ->
            ( State target hint "" [ renderTarget target ] False, Cmd.none )

        Update guess ->
            ( { state | input = guess }, Cmd.none )

        Guess ->
            if String.length state.word == String.length state.input then
                let
                    newResult =
                        markInput state.word state.input
                in
                ( { state
                    | input = ""
                    , guesses = state.guesses ++ [ newResult ]
                    , won = hasWon newResult
                  }
                , focusInput
                )

            else
                ( state, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none


green =
    rgb255 118 255 97


purple =
    rgb255 200 110 255


red =
    rgb255 255 100 84


grey =
    rgba255 97 113 117 48


statusToColour status =
    case status of
        Correct ->
            green

        Elsewhere ->
            purple

        Incorrect ->
            red

        Blank ->
            grey


renderGuess : Guess -> Element Msg
renderGuess guess =
    case guess of
        ( t, s ) ->
            el [ Background.color (statusToColour s) ] (text (String.fromChar t))


renderGuesses : List Guess -> Element Msg
renderGuesses guesses =
    row [ Font.family [ Font.monospace ], Font.size 32, padding 10, spacing 7 ] (map (\guess -> renderGuess guess) guesses)


view : State -> Document Msg
view state =
    { title = "haskle"
    , body =
        [ Element.layout [ Font.family [ Font.monospace ], Font.size 32 ]
            (column [ centerX, centerY ]
                ([ row [ Font.size 64 ] [ Element.link [] { url = "https://github.com/andimiller/haskle", label = Element.text "haskle" } ], Element.text state.hint ]
                    ++ map renderGuesses state.guesses
                    ++ [ row []
                            [ Element.html (input [ placeholder "Write guess here", id "guess-box", value state.input, onInput Update, onEnter Guess, disabled state.won ] [])
                            , Element.html (button [ onClick Reroll, hidden (not state.won) ] [ Html.text "Reroll" ])
                            ]
                       ]
                )
            )
        ]
    }
