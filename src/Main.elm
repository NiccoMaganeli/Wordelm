module Main exposing (main)

import Array
import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Random
import Toast
import Words as W


type alias Flags =
    { seed : Int }


type GameState
    = Guessing
    | Won
    | Loss


type alias ToastProperties =
    { msg : String, err : Bool }


type alias Model =
    { usrInp : String
    , attempts : List String
    , word : String
    , state : GameState
    , toast : Toast.Tray ToastProperties
    }


maxLetters : Int
maxLetters =
    5


maxAttempts : Int
maxAttempts =
    6


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
      , toast = Toast.tray
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


type Letter
    = Correct
    | Present
    | Absent


letterToString : Letter -> String
letterToString st =
    case st of
        Correct ->
            "correct"

        Present ->
            "present"

        Absent ->
            "absent"


type alias CharState =
    ( Char, Letter )


rebuildWord : List Char -> Dict Char Letter -> List CharState
rebuildWord chars dict =
    let
        notHandled : Char -> List CharState -> Bool
        notHandled c acc =
            acc
                |> List.map Tuple.first
                |> List.member c
                |> not
    in
    chars
        |> List.foldl
            (\c acc ->
                if notHandled c acc then
                    case Dict.get c dict of
                        Just st ->
                            acc ++ [ ( c, st ) ]

                        Nothing ->
                            acc ++ [ ( c, Absent ) ]

                else
                    acc ++ [ ( c, Absent ) ]
            )
            []


checkWord : String -> List Char -> List CharState
checkWord word chars =
    let
        wordList : List Char
        wordList =
            String.toList word

        evalChar : Char -> Char -> CharState
        evalChar c w =
            ( c
            , if c == w then
                Correct

              else if List.member c wordList then
                Present

              else
                Absent
            )

        isHandled : CharState -> Dict Char Letter -> Dict Char Letter
        isHandled ( c, st ) dict =
            case Dict.get c dict of
                Just _ ->
                    if st == Correct then
                        Dict.insert c st dict

                    else
                        dict

                Nothing ->
                    Dict.insert c st dict
    in
    wordList
        |> List.map2 evalChar chars
        |> List.foldl isHandled Dict.empty
        |> rebuildWord chars


flip : (a -> b -> c) -> b -> a -> c
flip fun a b =
    fun b a


renderInputs : Model -> Html Msg
renderInputs model =
    let
        tileWrapper : String -> List (Html Msg) -> Html Msg
        tileWrapper state =
            div [ class <| String.join " " [ "tile", state ] ]

        renderWord : List CharState -> List (Html Msg)
        renderWord chars =
            chars
                |> List.map
                    (\( c, st ) ->
                        tileWrapper (letterToString st) [ text <| String.fromChar c ]
                    )

        renderUserInput : Html Msg
        renderUserInput =
            div [ class "row-board" ]
                (if List.length model.attempts < maxAttempts then
                    model.usrInp
                        |> String.toList
                        |> List.map (\c -> [ text <| String.fromChar c ])
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
            |> List.map (\r -> div [ class "row-board" ] r)
         )
            ++ (renderUserInput :: renderEmptyWords)
        )


renderBoard : Model -> Html Msg
renderBoard model =
    div [ id "board-container" ]
        [ renderInputs model ]


renderKeyboard : Model -> Html Msg
renderKeyboard model =
    let
        knowSoFar : Dict Char Letter
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

        checkState : Maybe Letter -> List (Html.Attribute Msg)
        checkState maybeSt =
            case maybeSt of
                Nothing ->
                    []

                Just st ->
                    [ class <| letterToString st ]

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


mid : Model -> List (Html Msg)
mid model =
    [ renderBoard model
    , renderKeyboard model
    , renderToastContainer model
    ]


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
lossToast =
    persistentToast "Almost!" Loss


errToast : Model -> String -> ( Model, Cmd Toast.Msg )
errToast model msg =
    let
        ( toast, cmd ) =
            Toast.add model.toast (Toast.expireIn 2000 { msg = msg, err = True })
    in
    ( { model | toast = toast }, cmd )


update : Msg -> Model -> ( Model, Cmd Toast.Msg )
update msg model =
    case msg of
        OnClick c ->
            let
                allValidChar : List Char
                allValidChar =
                    "qwertyuiopasdfghjklzxcvbnm"
                        |> String.toList

                validChar : Char -> Bool
                validChar chr =
                    allValidChar
                        |> List.member chr
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
                        (case gmSt of
                            Won ->
                                winToast model

                            Loss ->
                                lossToast model

                            Guessing ->
                                ( model, Cmd.none )
                        )
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
        , update = \msg model -> Tuple.mapSecond (Cmd.map ToastMsg) (update msg model)
        , subscriptions = subscriptions
        }
