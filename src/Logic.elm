module Logic exposing (..)

import List exposing (filter, map, map2, member)
import String exposing (toList)
import Tuple exposing (first, pair, second)


type GuessStatus
    = Correct
    | Elsewhere
    | Incorrect
    | Blank


type alias Guess =
    ( Char, GuessStatus )

markCorrect : List Char -> List Char -> List Guess
markCorrect target guesses =
    map2
        (\t g ->
            if t == g then
                ( g, Correct )

            else
                ( g, Blank )
        )
        target
        guesses


markElsewhere : List Char -> List Guess -> List Guess
markElsewhere target guesses =
    let
        paired =
            map2 pair target guesses

        remaining =
            filter (second >> second >> (==) Correct >> not) paired

        remainingChars =
            map first remaining
    in
    map
        (\i ->
            case i of
                ( t, ( g, s ) ) ->
                    if s == Correct then
                        ( g, s )

                    else if member g remainingChars then
                        ( g, Elsewhere )

                    else
                        ( g, Incorrect )
        )
        paired


markInput : String -> String -> List Guess
markInput target guess =
    let
        targetChars =
            toList target

        guessChars =
            toList guess

        corrects =
            markCorrect targetChars guessChars
    in
    markElsewhere targetChars corrects


hasWon : List Guess -> Bool
hasWon =
    List.all (second >> (==) Correct)
