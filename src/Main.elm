module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, header, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type alias Model =
    { usrInp : String
    , oldInps : List String
    , word : String
    , stillGuessing : Bool
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { usrInp = "", oldInps = [], word = "gorge", stillGuessing = True }, Cmd.none )


top : Html Msg
top =
    header []
        [ div [] [ text "Help" ]
        , div [ class "title" ] [ text "Wordle" ]
        , div [] [ text "Configs" ]
        ]


type State
    = Correct
    | Present
    | Absent


eval : State -> String
eval st =
    case st of
        Correct ->
            "correct"

        Present ->
            "present"

        Absent ->
            "absent"


checkWord : String -> List Char -> List ( Char, State )
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

        renderWord : List ( Char, State ) -> List (Html Msg)
        renderWord chars =
            chars
                |> List.map
                    (\( c, st ) ->
                        tileWrapper (eval st) [ text <| String.fromChar c ]
                    )

        renderUserInput : Html Msg
        renderUserInput =
            div [ class "row-board" ]
                (if List.length model.oldInps < 6 then
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
            List.repeat (5 - List.length model.oldInps)
                (div [ class "row-board" ]
                    (List.repeat 5 (tileWrapper "empty" []))
                )
    in
    div [ class "board" ]
        ((model.oldInps
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
        knowSoFar : Dict Char State
        knowSoFar =
            model.oldInps
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

        checkState : Maybe State -> List (Html.Attribute Msg)
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
        , rowBtn <| txtBtn Submit "ENTER" :: renderRow "zxcvbnm" ++ [ txtBtn BckSpace "BKSP" ]
        ]


mid : Model -> List (Html Msg)
mid model =
    [ renderBoard model
    , renderKeyboard model
    ]


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
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        checkInput : Bool
        checkInput =
            model.usrInp
                |> String.toList
                |> List.map2 (==) (String.toList model.word)
                |> List.filter identity
                |> List.length
                |> (/=) 5

        validChar : List Char
        validChar =
            "qwertyuiopasdfghjklzxcvbnm"
                |> String.toList
    in
    case msg of
        OnClick c ->
            let
                inputIsFull : Bool
                inputIsFull =
                    String.length model.usrInp >= 5

                invalidChar : Bool
                invalidChar =
                    validChar
                        |> List.member c
                        |> not
            in
            ( { model
                | usrInp =
                    if not model.stillGuessing || invalidChar || inputIsFull then
                        model.usrInp

                    else
                        model.usrInp ++ String.fromChar c
              }
            , Cmd.none
            )

        Submit ->
            let
                wordHasFiveLetter : Bool
                wordHasFiveLetter =
                    String.length model.usrInp == 5

                lessThanSixOldInps : Bool
                lessThanSixOldInps =
                    List.length model.oldInps < 6
            in
            ( if model.stillGuessing && lessThanSixOldInps && wordHasFiveLetter then
                { model | oldInps = model.oldInps ++ [ model.usrInp ], usrInp = "", stillGuessing = checkInput }

              else
                model
            , Cmd.none
            )

        BckSpace ->
            ( { model | usrInp = String.dropRight 1 model.usrInp }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


keyDecoder : Decoder Msg
keyDecoder =
    let
        toKey : String -> Msg
        toKey str =
            case String.uncons str of
                Just ( c, "" ) ->
                    OnClick c

                _ ->
                    if str == "Backspace" then
                        BckSpace

                    else if str == "Enter" then
                        Submit

                    else
                        NoOp
    in
    Decode.string
        |> Decode.field "key"
        |> Decode.map toKey


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
