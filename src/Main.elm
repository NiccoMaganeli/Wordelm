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
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { usrInp = "", oldInps = [], word = "gorge" }, Cmd.none )


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
                if c == w then
                    ( c, Correct )

                else if List.member c wordList then
                    ( c, Present )

                else
                    ( c, Absent )
            )


flip : (a -> b -> c) -> b -> a -> c
flip fun a b =
    fun b a


renderInputs : List String -> String -> String -> Html Msg
renderInputs inputs userInp word =
    let
        renderWord : List ( Char, State ) -> List (Html Msg)
        renderWord chars =
            chars
                |> List.map
                    (\( c, st ) ->
                        div
                            [ class <|
                                String.join " "
                                    [ "tile"
                                    , eval st
                                    ]
                            ]
                            [ text <| String.fromChar c ]
                    )

        renderUserInput : Html Msg
        renderUserInput =
            div [ class "row-board" ]
                (if List.length inputs < 6 then
                    userInp
                        |> String.toList
                        |> List.map (\c -> [ text <| String.fromChar c ])
                        |> flip List.append (List.repeat (5 - String.length userInp) [])
                        |> List.map (\el -> div [ class "tile empty" ] el)

                 else
                    []
                )

        renderEmptyWords : List (Html Msg)
        renderEmptyWords =
            List.repeat (5 - List.length inputs) (div [ class "row-board" ] (List.repeat 5 (div [ class "tile empty" ] [])))
    in
    div [ class "board" ]
        ((inputs
            |> List.map (renderWord << checkWord word << String.toList)
            |> List.map (\r -> div [ class "row-board" ] r)
         )
            ++ (renderUserInput :: renderEmptyWords)
        )


renderBoard : Model -> Html Msg
renderBoard model =
    div [ class "board-container" ]
        [ renderInputs model.oldInps model.usrInp model.word
        ]


renderKeyboard : List String -> String -> Html Msg
renderKeyboard inputs word =
    let
        knowSoFar : Dict Char State
        knowSoFar =
            inputs
                |> List.concatMap (checkWord word << String.toList)
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
                    [ class <| eval st
                    ]

        renderRow : String -> List (Html Msg)
        renderRow row =
            row
                |> String.toList
                |> List.map (\c -> ( c, Dict.get c knowSoFar ))
                |> List.map
                    (\( c, maybeSt ) ->
                        button
                            (onClick (OnClick c) :: checkState maybeSt)
                            [ text <| String.fromChar c ]
                    )

        half : Html Msg
        half =
            div [ class "half" ] []

        txtBtn : Msg -> String -> Html Msg
        txtBtn msg txt =
            button [ class "one-and-a-half", onClick msg ] [ text txt ]
    in
    div [ id "keyboard" ]
        [ div [ class "row-button" ] (renderRow "qwertyuiop")
        , div [ class "row-button" ] (half :: renderRow "asdfghjkl" ++ [ half ])
        , div [ class "row-button" ] (txtBtn Submit "ENTER" :: renderRow "zxcvbnm" ++ [ txtBtn BckSpace "BKSP" ])
        ]


mid : Model -> Html Msg
mid model =
    div []
        [ renderBoard model
        , renderKeyboard model.oldInps model.word
        ]


view : Model -> Document Msg
view model =
    { title = "Wordle"
    , body =
        [ div [ class "main" ]
            [ top
            , mid model
            ]
        ]
    }


type Msg
    = OnClick Char
    | Submit
    | BckSpace
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClick c ->
            ( { model
                | usrInp =
                    if String.length model.usrInp >= 5 then
                        model.usrInp

                    else
                        model.usrInp ++ String.fromChar c
              }
            , Cmd.none
            )

        Submit ->
            ( if String.length model.usrInp == 5 then
                { model | oldInps = model.oldInps ++ [ model.usrInp ], usrInp = "" }

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
