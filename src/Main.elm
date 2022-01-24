port module Main exposing (main)

import Array
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h1, header, p, section, strong, text)
import Html.Attributes exposing (class, classList, id, style)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random
import Set exposing (Set)
import Task
import Time
import Toast
import Words as W


port saveGameHistory : String -> Cmd msg


port copyToClipboard : String -> Cmd msg


type alias GameHistory =
    { played : Int
    , perAttempts : List Int
    , seed : Int
    , attempts : List String
    , streak : Int
    , maxStreak : Int
    }


type alias Flags =
    { seed : Int
    , gameHistory : Maybe String
    }


type GameState
    = Guessing
    | Won
    | Loss


type Overlay
    = Help
    | Stats


gameHistoryDecoder : Decoder GameHistory
gameHistoryDecoder =
    Decode.map6 GameHistory
        (Decode.field "played" Decode.int)
        (Decode.field "perAttempts" (Decode.list Decode.int))
        (Decode.field "seed" Decode.int)
        (Decode.field "attempts" (Decode.list Decode.string))
        (Decode.field "streak" Decode.int)
        (Decode.field "maxStreak" Decode.int)


encodeGameHistory : Model -> String
encodeGameHistory { gameHistory, attempts, seed } =
    [ ( "played", Encode.int gameHistory.played )
    , ( "perAttempts", Encode.list Encode.int gameHistory.perAttempts )
    , ( "attempts", Encode.list Encode.string attempts )
    , ( "seed", Encode.int seed )
    , ( "streak", Encode.int gameHistory.streak )
    , ( "maxStreak", Encode.int gameHistory.maxStreak )
    ]
        |> Encode.object
        |> Encode.encode 0


type alias Model =
    { usrInp : String
    , attempts : List String
    , word : String
    , state : GameState
    , toast : Toast.Tray String
    , overlay : Maybe Overlay
    , gameHistory : GameHistory
    , seed : Int
    , time : Time.Posix
    , zone : Time.Zone
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
    , perAttempts = List.repeat maxAttempts 0
    , attempts = []
    , seed = 0
    , streak = 0
    , maxStreak = 0
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

        attempts : List String
        attempts =
            if gameHistory.seed == flags.seed then
                gameHistory.attempts

            else
                []

        state : GameState
        state =
            if List.any ((==) randomWord) attempts then
                Won

            else if List.length attempts == 6 then
                Loss

            else
                Guessing

        overlay : Maybe Overlay
        overlay =
            case state of
                Guessing ->
                    Nothing

                _ ->
                    Just Stats
    in
    ( { usrInp = ""
      , attempts = attempts
      , word = randomWord
      , state = state
      , toast = Toast.tray
      , overlay = overlay
      , gameHistory = gameHistory
      , seed = flags.seed
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


top : Html Msg
top =
    header []
        [ div [] [ button [ class "header-button", onClick <| OpenOverlay Help ] [ Icons.helpCircle ] ]
        , div [ class "title" ] [ text "Wordelm" ]
        , div [] [ button [ class "header-button", onClick <| OpenOverlay Stats ] [ Icons.barChart2 ] ]
        ]


type Letter
    = Correct Char
    | Present Char
    | Absent Char
    | NoState Char


count : Letter -> List Letter -> Int
count letter =
    List.length << List.filter ((==) letter)


safeGet : comparable -> b -> Dict comparable b -> b
safeGet val default =
    Maybe.withDefault default << Dict.get val


removeExtraPresent : Dict Char Int -> List Letter -> List Letter
removeExtraPresent occur letters =
    letters
        |> List.map
            (\letter ->
                case letter of
                    Present c ->
                        if count (Correct c) letters >= safeGet c 0 occur then
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
                        if count (Present c) acc >= safeGet c 0 occur then
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
flip fun b a =
    fun a b


tileWrapper : String -> List (Html Msg) -> Html Msg
tileWrapper state =
    let
        animate : Bool
        animate =
            [ "empty", "no-state" ]
                |> List.member state
                |> not
    in
    div [ classList [ ( "tile", True ), ( "animate", animate ), ( state, True ) ] ]


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
                        tileWrapper "correct" <| charText c

                    Present c ->
                        tileWrapper "present" <| charText c

                    Absent c ->
                        tileWrapper "absent" <| charText c

                    NoState c ->
                        tileWrapper "no-state" <| charText c
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
renderBoard =
    div [ id "board-container" ] << List.singleton << renderInputs


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
        renderToast : List (Html.Attribute Msg) -> Toast.Info String -> Html Msg
        renderToast _ toast =
            div [ class "toast" ] [ text toast.content ]
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
    div [ class "overlay help" ]
        [ div [ class "content-help" ]
            [ header [ class "help-header" ]
                [ h1 [] [ text "how to play" ]
                , button [ class "header-button", onClick CloseOverlay ] [ Icons.x ]
                ]
            , section []
                [ div [ class "instructions" ]
                    [ p [] [ text "Guess the ", strong [] [ text "WORDELM" ], text " in 6 tries." ]
                    , p [] [ text "Each guess must be a valid 5 letter word. Hit the enter button to submit." ]
                    , p [] [ text "After each guess, the color of the tiles will change to show how close your guess was to the word." ]
                    , div [ class "examples" ]
                        [ p [] [ strong [] [ text "Examples" ] ]
                        , renderExample (Correct 'w' :: (List.map NoState <| String.toList "eary")) 'W' CorrectEx
                        , renderExample (NoState 'p' :: Present 'i' :: (List.map NoState <| String.toList "lls")) 'I' PresentEx
                        , renderExample ((List.map NoState <| String.toList "vag") ++ [ Absent 'u', NoState 'e' ]) 'U' AbsentEx
                        ]
                    , p [] [ strong [] [ text "A new WORDELM will be available each day!" ] ]
                    ]
                ]
            ]
        ]


renderStatistics : GameHistory -> List (Html Msg)
renderStatistics { played, perAttempts, streak, maxStreak } =
    let
        winRatio : Int
        winRatio =
            if played == 0 then
                0

            else
                round <| (toFloat <| List.sum perAttempts) / toFloat played * 100
    in
    [ ( "Played", played )
    , ( "Win %", winRatio )
    , ( "Current Streak", streak )
    , ( "Max Streak", maxStreak )
    ]
        |> List.map
            (\( str, stat ) ->
                div [ class "statistic-container" ]
                    [ div [ class "statistic" ] [ text <| String.fromInt stat ]
                    , div [ class "label" ] [ text str ]
                    ]
            )


renderDistribution : Model -> List (Html Msg)
renderDistribution { gameHistory, attempts, state } =
    let
        gamesLost : Int
        gamesLost =
            gameHistory.played - List.sum gameHistory.perAttempts
    in
    gameHistory.perAttempts
        ++ [ gamesLost ]
        |> List.indexedMap
            (\i n ->
                let
                    width : String
                    width =
                        (toFloat n / toFloat gameHistory.played)
                            * 100
                            |> round
                            |> max 7
                            |> String.fromInt
                            |> flip (++) "%"

                    highlight : Bool
                    highlight =
                        case state of
                            Loss ->
                                i + 1 == 7

                            _ ->
                                i + 1 == List.length attempts

                    classes : Attribute Msg
                    classes =
                        classList
                            [ ( "graph-bar", True )
                            , ( "align-right", n > 0 )
                            , ( "highlight", highlight )
                            ]

                    guess : String
                    guess =
                        if i + 1 == 7 then
                            "x"

                        else
                            String.fromInt (i + 1)
                in
                div [ class "graph-container" ]
                    [ div [ class "guess" ] [ text guess ]
                    , div [ class "graph" ]
                        [ div [ classes, style "width" width ]
                            [ div [ class "num-guesses" ] [ text <| String.fromInt n ]
                            ]
                        ]
                    ]
            )


renderTimer : Time.Posix -> Time.Zone -> Html Msg
renderTimer time zone =
    let
        hours : Int
        hours =
            23 - Time.toHour zone time

        minutes : Int
        minutes =
            59 - Time.toMinute zone time

        seconds : Int
        seconds =
            59 - Time.toSecond zone time

        timerText : String
        timerText =
            [ hours, minutes, seconds ]
                |> List.map (String.padLeft 2 '0' << String.fromInt)
                |> String.join ":"
    in
    div [ id "timer" ]
        [ div [ class "statistic-container" ]
            [ div [ class "statistic timer" ] [ text timerText ] ]
        ]


renderFooter : Model -> Html Msg
renderFooter { time, zone } =
    div [ class "footer" ]
        [ div [ class "countdown" ] [ h1 [] [ text "Next Wordelm" ], renderTimer time zone ]
        , div [ class "share" ] [ button [ id "share-button", onClick ToClipboard ] [ text "Share", Icons.share2 ] ]
        ]


renderStats : Model -> Html Msg
renderStats ({ gameHistory, state } as model) =
    let
        footer : List (Html Msg)
        footer =
            case state of
                Guessing ->
                    []

                _ ->
                    [ renderFooter model ]
    in
    div [ class "overlay stats" ]
        [ div [ class "content-stats" ]
            [ button [ class "header-button close-icon", onClick CloseOverlay ] [ Icons.x ]
            , div [ class "stats-container" ]
                ([ h1 [] [ text "Statistics" ]
                 , div [ id "statistics" ] <| renderStatistics gameHistory
                 , h1 [] [ text "Guess Distribution" ]
                 , div [ id "distribution" ] <| renderDistribution model
                 ]
                    ++ footer
                )
            ]
        ]


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
    | OpenOverlay Overlay
    | CloseOverlay
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | ToClipboard


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


genToast : String -> Model -> ( Model, Cmd Toast.Msg )
genToast msg model =
    let
        ( toast, cmd ) =
            Toast.add model.toast (Toast.expireIn 2000 msg)
    in
    ( { model | toast = toast }, cmd )


winToast : Model -> ( Model, Cmd Toast.Msg )
winToast model =
    genToast "Congratulations!" { model | state = Won }


lossToast : Model -> ( Model, Cmd Toast.Msg )
lossToast model =
    genToast (String.join " " [ "Word was", model.word ++ "." ]) { model | state = Loss }


updateGameHistory : List Int -> Int -> Int -> Model -> Model
updateGameHistory perAttempts streak maxStreak ({ gameHistory } as model) =
    { model
        | gameHistory =
            { gameHistory
                | played = gameHistory.played + 1
                , perAttempts = perAttempts
                , streak = streak
                , maxStreak = maxStreak
            }
    }


updateWin : Model -> Model
updateWin ({ gameHistory } as model) =
    let
        newPerAttempts : List Int
        newPerAttempts =
            gameHistory.perAttempts
                |> List.indexedMap
                    (\i n ->
                        if (i + 1) == List.length model.attempts then
                            n + 1

                        else
                            n
                    )

        newStreak : Int
        newStreak =
            gameHistory.streak + 1

        newMaxStreak : Int
        newMaxStreak =
            if newStreak > gameHistory.maxStreak then
                newStreak

            else
                gameHistory.maxStreak
    in
    updateGameHistory newPerAttempts newStreak newMaxStreak model


updateLoss : Model -> Model
updateLoss ({ gameHistory } as model) =
    updateGameHistory gameHistory.perAttempts 0 gameHistory.maxStreak model


generateClipboard : Model -> String
generateClipboard { attempts, state, word } =
    let
        attemptsNumber : String
        attemptsNumber =
            if state == Loss then
                "X"

            else
                String.fromInt <| List.length attempts

        whichSquare : Letter -> String
        whichSquare letter =
            case letter of
                Absent _ ->
                    "⬛"

                Present _ ->
                    "🟨"

                Correct _ ->
                    "🟩"

                NoState _ ->
                    ""

        attemptsText : String
        attemptsText =
            attempts
                |> List.map (String.join "" << List.map whichSquare << checkWord word << String.toList)
                |> String.join "\n"
    in
    "Wordelm " ++ attemptsNumber ++ "/6\n\n" ++ attemptsText


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClick c ->
            ( { model
                | usrInp =
                    if not <| isGuessing model.state then
                        model.usrInp

                    else
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
                        model.usrInp
                            ++ String.fromChar c
                            |> String.filter validChar
                            |> String.slice 0 maxLetters
              }
            , Cmd.none
            )

        Submit ->
            let
                startNewRound : ( Model, Cmd Toast.Msg ) -> ( Model, Cmd Toast.Msg )
                startNewRound ( mdl, cmd ) =
                    ( { mdl | attempts = mdl.attempts ++ [ mdl.usrInp ], usrInp = "" }, cmd )
            in
            if isGuessing model.state then
                let
                    result : Result String GameState
                    result =
                        validateInput model.word model.attempts model.usrInp
                in
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
                                ( winModel
                                , Cmd.batch [ Cmd.map ToastMsg winCmd, saveGameHistory <| encodeGameHistory winModel ]
                                )

                            Loss ->
                                let
                                    ( lossModel, lossCmd ) =
                                        lossToast model
                                            |> startNewRound
                                            |> Tuple.mapFirst updateLoss
                                in
                                ( lossModel
                                , Cmd.batch [ Cmd.map ToastMsg lossCmd, saveGameHistory <| encodeGameHistory lossModel ]
                                )

                            Guessing ->
                                let
                                    ( guessingModel, guessingCmd ) =
                                        startNewRound ( model, Cmd.none )
                                in
                                ( guessingModel
                                , Cmd.batch [ Cmd.map ToastMsg guessingCmd, saveGameHistory <| encodeGameHistory guessingModel ]
                                )

                    Err str ->
                        genToast str model
                            |> Tuple.mapSecond (Cmd.map ToastMsg)

            else
                ( model, Cmd.none )

        BckSpace ->
            ( { model | usrInp = String.dropRight 1 model.usrInp }, Cmd.none )

        ToastMsg tmsg ->
            let
                ( toast, cmd ) =
                    Toast.update tmsg model.toast
            in
            ( { model | toast = toast }, Cmd.map ToastMsg cmd )

        OpenOverlay overlay ->
            ( { model | overlay = Just overlay }
            , case overlay of
                Help ->
                    Cmd.none

                Stats ->
                    Task.perform Tick Time.now
            )

        CloseOverlay ->
            ( { model | overlay = Nothing }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        ToClipboard ->
            let
                ( clipModel, clipCmd ) =
                    genToast "Copied to clipboard" model
            in
            ( clipModel, Cmd.batch [ copyToClipboard <| generateClipboard model, Cmd.map ToastMsg clipCmd ] )


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

        Just Stats ->
            Time.every 1000 Tick

        _ ->
            Sub.none


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
