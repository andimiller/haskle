module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Device, DeviceClass(..), Element, Orientation(..), alignRight, alignTop, centerX, centerY, clipX, column, el, fill, height, maximum, modular, padding, rgb255, rgba255, row, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, div, input)
import Html.Attributes exposing (disabled, hidden, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Json.Decode as Decode
import List exposing (map, map2, member)
import Logic exposing (..)
import Material.Icons.Action
import Material.Icons.Navigation
import Questions exposing (questions)
import Random
import Random.Extra exposing (sample)
import String exposing (toList)
import Task
import Tuple exposing (pair)
import Widget as Widget
import Widget.Customize as Customize
import Widget.Icon exposing (Icon)
import Widget.Material as Material


pickPuzzle =
    sample questions
        |> Random.map (Maybe.withDefault ( "hello", "world" ))


start : () -> ( State, Cmd Msg )
start () =
    ( State "" "" "" [] True { class = Desktop, orientation = Landscape }
    , Cmd.batch [ Random.generate Init pickPuzzle, detectSize ]
    )


main =
    Browser.document { init = start, update = update, view = view, subscriptions = subscriptions }


renderTarget : String -> List Guess
renderTarget word =
    map (\i -> ( '#', Blank )) (toList word)


type alias State =
    { word : String, hint : String, input : String, guesses : List (List Guess), won : Bool, device : Device }


type Msg
    = Reroll
    | Init ( String, String )
    | Guess
    | Update String
    | NoOp
    | Resize Int Int


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "guess-box")


detectSize : Cmd Msg
detectSize =
    Task.perform (\v -> Resize (truncate v.viewport.width) (truncate v.viewport.height)) Dom.getViewport


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Reroll ->
            start ()

        NoOp ->
            ( state, Cmd.none )

        Resize w h ->
            ( { state | device = Element.classifyDevice { width = w, height = w } }, Cmd.none )

        Init ( target, hint ) ->
            ( { state | word = target, hint = hint, input = "", guesses = [ renderTarget target ], won = False }, Cmd.none )

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
    onResize (\w h -> Resize w h)


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
    row [ Font.family [ Font.monospace ], padding 10, spacing 7 ] (map (\guess -> renderGuess guess) guesses)


sized : DeviceClass -> Int -> Int
sized dc s =
    let
        deviceFactor =
            case dc of
                Phone ->
                    2

                Tablet ->
                    2

                BigDesktop ->
                    2

                Desktop ->
                    1
    in
    truncate (deviceFactor * toFloat s)


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


theme =
    Material.defaultPalette


guessBox chosenTheme =
    let
        input =
            Material.textInput chosenTheme

        content =
            input.content

        input_elementRow =
            input.elementRow

        text =
            input.content.text

        new_text =
            { text | elementTextInput = text.elementTextInput ++ [ Input.focusedOnLoad, Element.htmlAttribute (id "guess-box"), onEnter Guess, width fill ] }

        new_content =
            { content | text = new_text }
    in
    { input | content = new_content, elementRow = input_elementRow ++ [ width fill ] }


view : State -> Document Msg
view state =
    { title = "haskle"
    , body =
        [ Element.layout [ Font.family [ Font.monospace ], Font.size (sized state.device.class 32) ]
            (column
                (if state.device.class == Desktop then
                    [ centerX, centerY ]

                 else
                    [ height shrink, width shrink, clipX ]
                )
                ([ wrappedRow [ Font.size (sized state.device.class 64) ] [ Element.link [] { url = "https://github.com/andimiller/haskle", label = Element.text "haskle" } ], Element.paragraph [] [ text state.hint ] ]
                    ++ map renderGuesses state.guesses
                    ++ [ row [ width fill ]
                            [ Widget.searchInput (guessBox theme)
                                { chips = []
                                , placeholder = Just (Input.placeholder [] (text "guess the name"))
                                , text = state.input
                                , onChange = Update
                                , label = "Guess the function name"
                                }
                            , if state.won then
                                Widget.button (Material.textButton theme) { icon = Material.Icons.Action.autorenew |> Widget.Icon.materialIcons, onPress = Just Reroll, text = "Reroll" }

                              else
                                Widget.button (Material.textButton theme) { icon = Material.Icons.Navigation.arrow_upward |> Widget.Icon.materialIcons, onPress = Just Guess, text = "Guess" }
                            ]
                       ]
                )
            )
        ]
    }
