module Main exposing (main)

import Array
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Random
import Words as W


type alias Flags =
    { seed : Int
    }


type GameState
    = Guessing
    | Won
    | Loss
    | Error String


type alias Model =
    { usrInp : String
    , attempts : List String
    , word : String
    , state : GameState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        randomInt : Random.Generator Int
        randomInt =
            Random.int 0 (List.length W.words - 1)

        randomWord : String
        randomWord =
            Random.step randomInt (Random.initialSeed flags.seed)
                |> Tuple.first
                |> flip Array.get (Array.fromList W.words)
                |> Maybe.withDefault "aeiou"
    in
    ( { usrInp = ""
      , attempts = []
      , word = randomWord
      , state = Guessing
      }
    , Cmd.none
    )


top : Html Msg
top =
    header []
        [ div [] [ text "Help" ]
        , div [ class "title" ] [ text "Wordle" ]
        , div [] [ text "Configs" ]
        ]


type LetterState
    = Correct
    | Present
    | Absent


eval : LetterState -> String
eval st =
    case st of
        Correct ->
            "correct"

        Present ->
            "present"

        Absent ->
            "absent"


checkWord : String -> List Char -> List ( Char, LetterState )
checkWord word chars =
    let
        wordList : List Char
        wordList =
            String.toList word
    in
    wordList
        |> List.map2 Tuple.pair chars
        |> List.map
            (\( c, w ) ->
                ( c
                , if c == w then
                    Correct

                  else if List.member c wordList then
                    Present

                  else
                    Absent
                )
            )


flip : (a -> b -> c) -> b -> a -> c
flip fun a b =
    fun b a


renderInputs : Model -> Html Msg
renderInputs model =
    let
        tileWrapper : String -> List (Html Msg) -> Html Msg
        tileWrapper state =
            div [ class <| String.join " " [ "tile", state ] ]

        renderWord : List ( Char, LetterState ) -> List (Html Msg)
        renderWord chars =
            chars
                |> List.map
                    (\( c, st ) ->
                        tileWrapper (eval st) [ text <| String.fromChar c ]
                    )

        renderUserInput : Html Msg
        renderUserInput =
            div [ class "row-board" ]
                (if List.length model.attempts < 6 then
                    model.usrInp
                        |> String.toList
                        |> List.map (\c -> [ text <| String.fromChar c ])
                        |> flip List.append (List.repeat (5 - String.length model.usrInp) [])
                        |> List.map (tileWrapper "empty")

                 else
                    []
                )

        renderEmptyWords : List (Html Msg)
        renderEmptyWords =
            List.repeat (5 - List.length model.attempts)
                (div [ class "row-board" ]
                    (List.repeat 5 (tileWrapper "empty" []))
                )
    in
    div [ class "board" ]
        ((model.attempts
            |> List.map (renderWord << checkWord model.word << String.toList)
            |> List.map (\r -> div [ class "row-board" ] r)
         )
            ++ (renderUserInput :: renderEmptyWords)
        )


renderBoard : Model -> Html Msg
renderBoard model =
    div [ id "board-container" ]
        [ renderInputs model
        ]


renderKeyboard : Model -> Html Msg
renderKeyboard model =
    let
        knowSoFar : Dict Char LetterState
        knowSoFar =
            model.attempts
                |> List.concatMap (checkWord model.word << String.toList)
                |> List.foldl
                    (\( c, st ) d ->
                        case Dict.get c d of
                            Nothing ->
                                Dict.insert c st d

                            Just oldSt ->
                                if oldSt == Correct then
                                    d

                                else
                                    Dict.insert c st d
                    )
                    Dict.empty

        checkState : Maybe LetterState -> List (Html.Attribute Msg)
        checkState maybeSt =
            case maybeSt of
                Nothing ->
                    []

                Just st ->
                    [ class <| eval st ]

        renderRow : String -> List (Html Msg)
        renderRow row =
            row
                |> String.toList
                |> List.map
                    (\c ->
                        button
                            (onClick (OnClick c) :: checkState (Dict.get c knowSoFar))
                            [ text <| String.fromChar c ]
                    )

        rowBtn : List (Html Msg) -> Html Msg
        rowBtn =
            div [ class "row-button" ]

        half : Html Msg
        half =
            div [ class "half" ] []

        txtBtn : Msg -> String -> Html Msg
        txtBtn msg txt =
            button [ class "one-and-a-half", onClick msg ] [ text txt ]
    in
    div [ id "keyboard" ]
        [ rowBtn <| renderRow "qwertyuiop"
        , rowBtn <| half :: renderRow "asdfghjkl" ++ [ half ]
        , rowBtn <| txtBtn Submit "⏎" :: renderRow "zxcvbnm" ++ [ txtBtn BckSpace "⌫" ]
        ]


renderAlert : String -> Html Msg
renderAlert msg =
    div [ class "toaster" ] [ text msg ]


mid : Model -> List (Html Msg)
mid model =
    [ renderBoard model
    , renderKeyboard model
    ]
        ++ (case model.state of
                Error msg ->
                    [ renderAlert msg ]

                Won ->
                    [ renderAlert "Congratulations!" ]

                Loss ->
                    [ renderAlert "Almost!" ]

                Guessing ->
                    []
           )


view : Model -> Document Msg
view model =
    { title = "Wordle"
    , body =
        [ div [ id "main" ]
            (top :: mid model)
        ]
    }


type Msg
    = OnClick Char
    | Submit
    | BckSpace


isGuessing : GameState -> Bool
isGuessing gmSt =
    case gmSt of
        Guessing ->
            True

        Error _ ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClick c ->
            let
                inputIsFull : Bool
                inputIsFull =
                    String.length model.usrInp == 5

                validChar : List Char
                validChar =
                    "qwertyuiopasdfghjklzxcvbnm"
                        |> String.toList

                invalidChar : Bool
                invalidChar =
                    validChar
                        |> List.member c
                        |> not
            in
            ( { model
                | usrInp =
                    if (not <| isGuessing model.state) || inputIsFull || invalidChar then
                        model.usrInp

                    else
                        model.usrInp ++ String.fromChar c
              }
            , Cmd.none
            )

        Submit ->
            let
                lessThanSixAttempts : Bool
                lessThanSixAttempts =
                    List.length model.attempts < 6

                wordHasFiveLetter : Maybe String
                wordHasFiveLetter =
                    if String.length model.usrInp == 5 then
                        Nothing

                    else
                        Just "Not enough letters"

                validWord : Maybe String
                validWord =
                    if List.member model.usrInp W.words then
                        Nothing

                    else
                        Just "Not a valid word"

                haveError : Maybe String
                haveError =
                    [ wordHasFiveLetter, validWord ]
                        |> List.Extra.dropWhile ((==) Nothing)
                        |> List.head
                        |> Maybe.withDefault Nothing

                checkInput : GameState
                checkInput =
                    if model.usrInp == model.word then
                        Won

                    else if List.length model.attempts == 6 then
                        Loss

                    else
                        Guessing
            in
            ( if isGuessing model.state && lessThanSixAttempts then
                if haveError /= Nothing then
                    { model | state = Error <| Maybe.withDefault "" haveError }

                else
                    { model | attempts = model.attempts ++ [ model.usrInp ], usrInp = "", state = checkInput }

              else
                model
            , Cmd.none
            )

        BckSpace ->
            ( { model | usrInp = String.dropRight 1 model.usrInp }, Cmd.none )


keyDecoder : Decoder Msg
keyDecoder =
    let
        toKey : String -> Decoder Msg
        toKey str =
            case String.uncons str of
                Just ( c, "" ) ->
                    Decode.succeed <| OnClick c

                _ ->
                    if str == "Backspace" then
                        Decode.succeed BckSpace

                    else if str == "Enter" then
                        Decode.succeed Submit

                    else
                        Decode.fail "invalid char"
    in
    Decode.string
        |> Decode.field "key"
        |> Decode.andThen toKey


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
