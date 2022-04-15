module LogicSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Logic exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Logic"
        [ describe "markInput"
            [ test "should mark correct input as correct" <|
                \_ ->
                    let
                        result =
                            markInput "hello" "hello"
                    in
                    Expect.equal result [ ( 'h', Correct ), ( 'e', Correct ), ( 'l', Correct ), ( 'l', Correct ), ( 'o', Correct ) ]
            , test "should mark incorrect input as incorrect" <|
                \_ ->
                    let
                        result =
                            markInput "aaa" "bbb"
                    in
                    Expect.equal result [ ( 'b', Incorrect ), ( 'b', Incorrect ), ( 'b', Incorrect ) ]
            , test "should detect wrongly placed characters" <|
                \_ ->
                    let
                        result =
                            markInput "ab" "ba"
                    in
                    Expect.equal result [ ( 'b', Elsewhere ), ( 'a', Elsewhere ) ]
            , test "should detect wrongly placed characters but not repeated ones" <|
                \_ ->
                    let
                        result =
                            markInput "ab" "bb"
                    in
                    Expect.equal result [ ( 'b', Incorrect ), ( 'b', Correct ) ]
            , test "should detect wrongly placed characters but not repeated ones 2" <|
                \_ ->
                    let
                        result =
                            markInput "abb" "bba"
                    in
                    Expect.equal result [ ( 'b', Elsewhere ), ( 'b', Correct ), ( 'a', Elsewhere ) ]
            ]
        , describe "hasWon"
            [ test "says they've won if all are correct" <|
                \_ ->
                    Expect.equal (hasWon [ ( 'a', Correct ), ( 'b', Correct ) ]) True
            , test "says the haven't won if some are not correct" <|
                \_ ->
                    Expect.equal (hasWon [ ( 'a', Correct ), ( 'b', Incorrect ) ]) False
            , test "says the haven't won if some are blank" <|
                \_ ->
                    Expect.equal (hasWon [ ( 'a', Correct ), ( 'b', Blank ) ]) False
            , test "says the haven't won if some are misplaced" <|
                \_ ->
                    Expect.equal (hasWon [ ( 'a', Elsewhere ), ( 'b', Elsewhere ) ]) False
            ]
        ]
