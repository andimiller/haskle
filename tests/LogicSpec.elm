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
                    Expect.equal (markInput "hello" "hello") [ ( 'h', Correct ), ( 'e', Correct ), ( 'l', Correct ), ( 'l', Correct ), ( 'o', Correct ) ]
            , test "should mark incorrect input as incorrect" <|
                \_ ->
                    Expect.equal (markInput "aaa" "bbb") [ ( 'b', Incorrect ), ( 'b', Incorrect ), ( 'b', Incorrect ) ]
            , test "should detect wrongly placed characters" <|
                \_ ->
                    Expect.equal (markInput "ab" "ba")[ ( 'b', Elsewhere ), ( 'a', Elsewhere ) ]
            , test "should detect wrongly placed characters but not repeated ones" <|
                \_ ->
                    Expect.equal (markInput "ab" "bb")[ ( 'b', Incorrect ), ( 'b', Correct ) ]
            , test "should detect wrongly placed characters but not repeated ones 2" <|
                \_ ->
                    Expect.equal (markInput "abb" "bba") [ ( 'b', Elsewhere ), ( 'b', Correct ), ( 'a', Elsewhere ) ]
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
