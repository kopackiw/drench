module Drench exposing (main)

import Browser
import Game
    exposing
        ( BoardSize
        , Color
        , GameResult(..)
        , GameState
        , GameStatus(..)
        , PlayingGameMsg(..)
        , colorToString
        , createNewGame
        , generateRandomBoardWithSize
        , initialBoardSize
        , updateGameStatus
        )
import Html exposing (Html, button, div, input, li, ol, p, text)
import Html.Attributes exposing (class, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Random



------ MODEL ------


type Model
    = LoadingGame
    | GameLoaded GameStatus


type Msg
    = LoadingGameMsg LoadingGameMsg
    | PlayingGameMsg PlayingGameMsg
    | ChangingConfigMsg ChangingConfigMsg


type LoadingGameMsg
    = BoardCreated (List Color)


type ChangingConfigMsg
    = NewGameClicked


initialModel : Model
initialModel =
    LoadingGame



------ UPDATE ------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingGameMsg (BoardCreated colors), _ ) ->
            ( GameLoaded (createNewGame colors), Cmd.none )

        ( PlayingGameMsg (ColorChanged newColor), GameLoaded gameStatus ) ->
            ( GameLoaded (updateGameStatus (ColorChanged newColor) gameStatus), Cmd.none )

        ( PlayingGameMsg (BoardSizeChanged newSize), GameLoaded gameStatus ) ->
            ( GameLoaded (updateGameStatus (BoardSizeChanged newSize) gameStatus)
            , runNewBoardGenerationWithSize newSize
            )

        ( ChangingConfigMsg NewGameClicked, GameLoaded (GameInProgress gameStatus) ) ->
            ( model, runNewBoardGenerationWithSize gameStatus.boardSize )

        ( ChangingConfigMsg NewGameClicked, GameLoaded (GameFinished gameStatus _) ) ->
            ( model, runNewBoardGenerationWithSize gameStatus.boardSize )

        _ ->
            ( model, Cmd.none )


runNewBoardGenerationWithSize : BoardSize -> Cmd Msg
runNewBoardGenerationWithSize =
    Random.generate (LoadingGameMsg << BoardCreated) << generateRandomBoardWithSize



------ VIEW ------


view : Model -> Html Msg
view model =
    case model of
        LoadingGame ->
            div [] [ text "Loading..." ]

        GameLoaded (GameInProgress gameState) ->
            div
                [ class "wrapper" ]
                [ viewMenu gameState
                , viewBoard gameState
                , viewInstructions
                ]

        GameLoaded (GameFinished _ gameResult) ->
            div
                [ class "wrapper" ]
                [ viewNewGameButton
                , viewResult gameResult
                ]


viewBoard : GameState -> Html Msg
viewBoard gameState =
    div
        [ class "board"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt gameState.boardSize ++ ", 1fr)")
        ]
        (List.map viewCell gameState.board)


viewCell : Color -> Html Msg
viewCell color =
    div
        [ class "cell"
        , class (colorToString color)
        , onClick ((PlayingGameMsg << ColorChanged) color)
        ]
        []


viewMenu : GameState -> Html Msg
viewMenu gameState =
    div
        [ class "menu" ]
        [ viewUpdateSizeRange gameState.boardSize
        , viewNewGameButton
        , viewPlayersMoves gameState.playerMovesCounter
        ]


viewUpdateSizeRange : BoardSize -> Html Msg
viewUpdateSizeRange boardSize =
    div
        [ class "sliderWrapper" ]
        [ p
            [ class "sliderLabel", class "label" ]
            [ text ("Board size: " ++ String.fromInt boardSize) ]
        , input
            [ onInput (PlayingGameMsg << BoardSizeChanged << withDefault boardSize << String.toInt)
            , class "slider"
            , type_ "range"
            , Html.Attributes.min "6"
            , Html.Attributes.max "28"
            , value (String.fromInt boardSize)
            , step "1"
            ]
            []
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    button
        [ onClick (ChangingConfigMsg NewGameClicked)
        , class "newGameButton"
        ]
        [ text "New game" ]


viewPlayersMoves : Int -> Html Msg
viewPlayersMoves moves =
    p
        [ class "label" ]
        [ text ("Remaining moves: " ++ String.fromInt moves) ]


viewResult : GameResult -> Html Msg
viewResult gameResult =
    let
        textToDisplay =
            case gameResult of
                Win ->
                    "Congrats! 🎉 Wanna play again?"

                Loose ->
                    "Keep trying and play again! 💪🏼"
    in
    div
        []
        [ text textToDisplay ]


viewInstructions : Html Msg
viewInstructions =
    ol
        [ class "instructions" ]
        [ li [] [ text "To win you must drench whole board with single color." ]
        , li [] [ text "You have limit of moves." ]
        , li [] [ text "Your current color is always at the top left corner." ]
        , li [] [ text "Change current color to a new one by clicking on any square with new color." ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, runNewBoardGenerationWithSize initialBoardSize )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
