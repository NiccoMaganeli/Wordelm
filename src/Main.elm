port module Main exposing (main)

import Array
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, header, p, section, strong, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Set exposing (Set)
import Toast
import Words as W


port saveGameHistory : String -> Cmd msg


type alias GameHistory =
    { played : Int, perAttempts : List Int }


type alias Flags =
    { seed : Int
    , gameHistory : Maybe String
    }


type GameState
    = Guessing
    | Won
    | Loss


type alias ToastProperties =
    { msg : String, err : Bool }


type Overlay
    = Help
    | Stats


gameHistoryDecoder : Decoder GameHistory
gameHistoryDecoder =
    Decode.map2 GameHistory
        (Decode.field "played" Decode.int)
        (Decode.field "perAttempts" (Decode.list Decode.int))


encodeGameHistory : GameHistory -> String
encodeGameHistory gameHistory =
    [ ( "played", Encode.int gameHistory.played )
    , ( "perAttempts", Encode.list Encode.int gameHistory.perAttempts )
    ]
        |> Encode.object
        |> Encode.encode 0


type alias Model =
    { usrInp : String
    , attempts : List String
    , word : String
    , state : GameState
    , toast : Toast.Tray ToastProperties
    , overlay : Maybe Overlay
    , gameHistory : GameHistory
    }


maxLetters : Int
maxLetters =
    5


maxAttempts : Int
maxAttempts =
    6


initGameHistory : GameHistory
initGameHistory =
    { played = 0
    , perAttempts = List.repeat 6 0
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

        gameHistory : GameHistory
        gameHistory =
            flags.gameHistory
                |> Maybe.andThen (Result.toMaybe << Decode.decodeString gameHistoryDecoder)
                |> Maybe.withDefault initGameHistory
    in
    ( { usrInp = ""
      , attempts = []
      , word = randomWord
      , state = Guessing
      , toast = Toast.tray
      , overlay = Nothing
      , gameHistory = gameHistory
      }
    , Cmd.none
    )


top : Html Msg
top =
    header []
        [ div [] [ button [ class "header-button", onClick OpenHelp ] [ Icons.helpCircle ] ]
        , div [ class "title" ] [ text "Wordle" ]
        , div [] [ button [ class "header-button", onClick OpenStats ] [ Icons.barChart2 ] ]
        ]


type Letter
    = Correct Char
    | Present Char
    | Absent Char
    | NoState Char


count : (Char -> Letter) -> List Letter -> Int
count constructor letters =
    letters
        |> List.filter (\letter -> letter == (constructor <| letterChar letter))
        |> List.length


countCorrect : List Letter -> Int
countCorrect =
    count Correct


countPresent : List Letter -> Int
countPresent =
    count Present


safeGet : comparable -> b -> Dict comparable b -> b
safeGet val default dict =
    dict
        |> Dict.get val
        |> Maybe.withDefault default


removeExtraPresent : Dict Char Int -> List Letter -> List Letter
removeExtraPresent occur letters =
    letters
        |> List.map
            (\letter ->
                case letter of
                    Present c ->
                        if countCorrect letters >= safeGet c 0 occur then
                            Absent c

                        else
                            letter

                    _ ->
                        letter
            )


removePresent : Dict Char Int -> List Letter -> List Letter
removePresent occur =
    List.reverse
        << List.foldl
            (\letter acc ->
                case letter of
                    Present c ->
                        if countPresent acc >= safeGet c 0 occur then
                            Absent c :: acc

                        else
                            letter :: acc

                    _ ->
                        letter :: acc
            )
            []


checkWord : String -> List Char -> List Letter
checkWord word chars =
    let
        wordList : List Char
        wordList =
            String.toList word

        evalChar : Char -> Char -> Letter
        evalChar c w =
            (if c == w then
                Correct

             else if List.member c wordList then
                Present

             else
                Absent
            )
                c

        increase : Maybe Int -> Maybe Int
        increase =
            Just << (+) 1 << Maybe.withDefault 0

        occurences : Dict Char Int
        occurences =
            List.foldl (flip Dict.update increase) Dict.empty wordList
    in
    wordList
        |> List.map2 evalChar chars
        |> removeExtraPresent occurences
        |> removePresent occurences


flip : (a -> b -> c) -> b -> a -> c
flip fun a b =
    fun b a


tileWrapper : String -> List (Html Msg) -> Html Msg
tileWrapper state =
    div [ class <| String.join " " [ "tile", state ] ]


charText : Char -> List (Html Msg)
charText =
    List.singleton << text << String.fromChar


renderWord : List Letter -> List (Html Msg)
renderWord chars =
    chars
        |> List.map
            (\letter ->
                case letter of
                    Correct c ->
                        tileWrapper "correct" (charText c)

                    Present c ->
                        tileWrapper "present" (charText c)

                    Absent c ->
                        tileWrapper "absent" (charText c)

                    NoState c ->
                        tileWrapper "no-state" (charText c)
            )


renderInputs : Model -> Html Msg
renderInputs model =
    let
        renderUserInput : Html Msg
        renderUserInput =
            div [ class "row-board" ]
                (if List.length model.attempts < maxAttempts then
                    model.usrInp
                        |> String.toList
                        |> List.map charText
                        |> flip List.append (List.repeat (maxLetters - String.length model.usrInp) [])
                        |> List.map (tileWrapper "empty")

                 else
                    []
                )

        renderEmptyWords : List (Html Msg)
        renderEmptyWords =
            List.repeat (maxAttempts - 1 - List.length model.attempts)
                (div [ class "row-board" ]
                    (List.repeat maxLetters (tileWrapper "empty" []))
                )
    in
    div [ class "board" ]
        ((model.attempts
            |> List.map (renderWord << checkWord model.word << String.toList)
            |> List.map (div [ class "row-board" ])
         )
            ++ (renderUserInput :: renderEmptyWords)
        )


renderBoard : Model -> Html Msg
renderBoard model =
    div [ id "board-container" ]
        [ renderInputs model ]


letterChar : Letter -> Char
letterChar letter =
    case letter of
        Correct c ->
            c

        Present c ->
            c

        Absent c ->
            c

        NoState c ->
            c


isCorrect : Letter -> Bool
isCorrect letter =
    case letter of
        Correct _ ->
            True

        _ ->
            False


renderKeyboard : Model -> Html Msg
renderKeyboard model =
    let
        knowSoFar : Dict Char Letter
        knowSoFar =
            model.attempts
                |> List.concatMap (checkWord model.word << String.toList)
                |> List.foldl
                    (\letter d ->
                        let
                            chr : Char
                            chr =
                                letterChar letter
                        in
                        case Dict.get chr d of
                            Nothing ->
                                Dict.insert chr letter d

                            Just oldLetter ->
                                if isCorrect oldLetter then
                                    d

                                else
                                    Dict.insert chr letter d
                    )
                    Dict.empty

        checkState : Maybe Letter -> List (Html.Attribute Msg)
        checkState maybeSt =
            case maybeSt of
                Nothing ->
                    []

                Just st ->
                    (case st of
                        Correct _ ->
                            "correct"

                        Present _ ->
                            "present"

                        Absent _ ->
                            "absent"

                        NoState _ ->
                            "no-state"
                    )
                        |> class
                        |> List.singleton

        renderRow : String -> List (Html Msg)
        renderRow row =
            row
                |> String.toList
                |> List.map
                    (\c ->
                        button
                            (onClick (OnClick c) :: class "keyboard-button" :: checkState (Dict.get c knowSoFar))
                            (charText c)
                    )

        rowBtn : List (Html Msg) -> Html Msg
        rowBtn =
            div [ class "row-button" ]

        half : Html Msg
        half =
            div [ class "half" ] []

        txtBtn : Msg -> String -> Html Msg
        txtBtn msg txt =
            button [ class "one-and-a-half keyboard-button", onClick msg ] [ text txt ]
    in
    div [ id "keyboard" ]
        [ rowBtn <| renderRow "qwertyuiop"
        , rowBtn <| half :: renderRow "asdfghjkl" ++ [ half ]
        , rowBtn <| txtBtn Submit "⏎" :: renderRow "zxcvbnm" ++ [ txtBtn BckSpace "⌫" ]
        ]


toastConfig : Toast.Config Msg
toastConfig =
    Toast.config ToastMsg


renderToastContainer : Model -> Html Msg
renderToastContainer model =
    let
        renderToast : List (Html.Attribute Msg) -> Toast.Info ToastProperties -> Html Msg
        renderToast _ toast =
            div
                [ class
                    (if toast.content.err then
                        "err-toast"

                     else
                        "toast"
                    )
                ]
                [ text toast.content.msg ]
    in
    div [ class "toast-container" ] [ Toast.render renderToast model.toast toastConfig ]


type ExampleType
    = CorrectEx
    | PresentEx
    | AbsentEx


renderExample : List Letter -> Char -> ExampleType -> Html Msg
renderExample letters c example =
    let
        exampleType : String
        exampleType =
            case example of
                CorrectEx ->
                    " is in the word and in the correct spot."

                PresentEx ->
                    " is in the word but in the wrong spot."

                AbsentEx ->
                    " is not in the word in any spot."
    in
    div [ class "example" ]
        [ div [ class "row-board" ] (renderWord letters)
        , p [] [ text "The letter ", strong [] (charText c), text exampleType ]
        ]


renderHelp : Html Msg
renderHelp =
    div [ class "help-overlay" ]
        [ div [ class "content" ]
            [ header [ class "help-header" ]
                [ h1 [] [ text "how to play" ]
                , button [ class "header-button", onClick CloseOverlay ] [ Icons.x ]
                ]
            , section []
                [ div [ class "instructions" ]
                    [ p [] [ text "Guess the ", strong [] [ text "WORDLE" ], text " in 6 tries." ]
                    , p [] [ text "Each guess must be a valid 5 letter word. Hit the enter button to submit." ]
                    , p [] [ text "After each guess, the color of the tiles will change to show how close your guess was to the word." ]
                    , div [ class "examples" ]
                        [ p [] [ strong [] [ text "Examples" ] ]
                        , renderExample (Correct 'w' :: (List.map NoState <| String.toList "eary")) 'W' CorrectEx
                        , renderExample (NoState 'p' :: Present 'i' :: (List.map NoState <| String.toList "lls")) 'I' PresentEx
                        , renderExample ((List.map NoState <| String.toList "vag") ++ [ Absent 'u', NoState 'e' ]) 'U' AbsentEx
                        ]
                    , p [] [ strong [] [ text "A new WORDLE will be available each day!" ] ]
                    ]
                ]
            ]
        ]


renderStats : Model -> Html Msg
renderStats _ =
    div [] []


mid : Model -> List (Html Msg)
mid model =
    [ renderBoard model
    , renderKeyboard model
    , renderToastContainer model
    ]
        ++ (case model.overlay of
                Just overlay ->
                    case overlay of
                        Help ->
                            [ renderHelp ]

                        Stats ->
                            [ renderStats model ]

                Nothing ->
                    []
           )


view : Model -> Document Msg
view model =
    { title = "Wordelm"
    , body =
        [ div [ id "main" ]
            (top :: mid model)
        ]
    }


type Msg
    = OnClick Char
    | Submit
    | BckSpace
    | ToastMsg Toast.Msg
    | OpenHelp
    | OpenStats
    | CloseOverlay


isGuessing : GameState -> Bool
isGuessing gmSt =
    case gmSt of
        Guessing ->
            True

        _ ->
            False


validateInput : String -> List String -> String -> Result String GameState
validateInput word attempts inp =
    if String.length inp < maxLetters then
        Err "Not enough letters"

    else if not <| List.member inp W.words then
        Err "Not a valid word"

    else if word == inp then
        Ok Won

    else if List.length attempts == (maxAttempts - 1) then
        Ok Loss

    else
        Ok Guessing


persistentToast : String -> GameState -> Model -> ( Model, Cmd Toast.Msg )
persistentToast msg gmSt model =
    let
        ( toast, cmd ) =
            Toast.add model.toast (Toast.persistent { msg = msg, err = False })
    in
    ( { model | toast = toast, state = gmSt }, cmd )


winToast : Model -> ( Model, Cmd Toast.Msg )
winToast =
    persistentToast "Congratulations!" Won


lossToast : Model -> ( Model, Cmd Toast.Msg )
lossToast model =
    persistentToast (String.join " " [ "Word was", model.word ++ "." ]) Loss model


errToast : Model -> String -> ( Model, Cmd Toast.Msg )
errToast model msg =
    let
        ( toast, cmd ) =
            Toast.add model.toast (Toast.expireIn 2000 { msg = msg, err = True })
    in
    ( { model | toast = toast }, cmd )


updateGameHistory : Int -> List Int -> Model -> Model
updateGameHistory played perAttempts model =
    { model | gameHistory = { played = played + 1, perAttempts = perAttempts } }


updateWin : Model -> Model
updateWin model =
    let
        newPerAttempts : List Int
        newPerAttempts =
            model.gameHistory.perAttempts
                |> List.indexedMap
                    (\i n ->
                        if (i + 1) == List.length model.attempts then
                            n + 1

                        else
                            n
                    )
    in
    updateGameHistory model.gameHistory.played newPerAttempts model


updateLoss : Model -> Model
updateLoss model =
    updateGameHistory model.gameHistory.played model.gameHistory.perAttempts model


update : Msg -> Model -> ( Model, Cmd Toast.Msg )
update msg model =
    case msg of
        OnClick c ->
            let
                allValidChar : Set Char
                allValidChar =
                    "qwertyuiopasdfghjklzxcvbnm"
                        |> String.toList
                        |> Set.fromList

                validChar : Char -> Bool
                validChar =
                    flip Set.member allValidChar
            in
            ( { model
                | usrInp =
                    if not <| isGuessing model.state then
                        model.usrInp

                    else
                        model.usrInp
                            ++ String.fromChar c
                            |> String.filter validChar
                            |> String.slice 0 maxLetters
              }
            , Cmd.none
            )

        Submit ->
            let
                result : Result String GameState
                result =
                    validateInput model.word model.attempts model.usrInp

                startNewRound : ( Model, Cmd Toast.Msg ) -> ( Model, Cmd Toast.Msg )
                startNewRound ( mdl, cmd ) =
                    ( { mdl | attempts = mdl.attempts ++ [ mdl.usrInp ], usrInp = "" }, cmd )
            in
            if isGuessing model.state then
                case result of
                    Ok gmSt ->
                        case gmSt of
                            Won ->
                                let
                                    ( winModel, winCmd ) =
                                        winToast model
                                            |> startNewRound
                                            |> Tuple.mapFirst updateWin
                                in
                                ( winModel, Cmd.batch [ winCmd, saveGameHistory <| encodeGameHistory winModel.gameHistory ] )

                            Loss ->
                                let
                                    ( lossModel, lossCmd ) =
                                        lossToast model
                                            |> startNewRound
                                            |> Tuple.mapFirst updateLoss
                                in
                                ( lossModel, Cmd.batch [ lossCmd, saveGameHistory <| encodeGameHistory lossModel.gameHistory ] )

                            Guessing ->
                                ( model, Cmd.none )
                                    |> startNewRound

                    Err str ->
                        errToast model str

            else
                ( model, Cmd.none )

        BckSpace ->
            ( { model | usrInp = String.dropRight 1 model.usrInp }, Cmd.none )

        ToastMsg tmsg ->
            let
                ( toast, cmd ) =
                    Toast.update tmsg model.toast
            in
            ( { model | toast = toast }, cmd )

        OpenHelp ->
            ( { model | overlay = Just Help }, Cmd.none )

        OpenStats ->
            ( { model | overlay = Just Stats }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = Nothing }, Cmd.none )


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
subscriptions model =
    case model.overlay of
        Nothing ->
            Browser.Events.onKeyDown keyDecoder

        Just _ ->
            Sub.none


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = \msg model -> Tuple.mapSecond (Cmd.map ToastMsg) (update msg model)
        , subscriptions = subscriptions
        }
