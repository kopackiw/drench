module Game exposing
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

import List.Extra exposing (count, unique, updateIf)
import Maybe exposing (withDefault)
import Random



------ MODEL ------


type GameStatus
    = GameInProgress GameState
    | GameFinished GameState GameResult


type alias GameState =
    { board : Board
    , currentColor : Color
    , boardSize : BoardSize
    , playerMovesCounter : Int
    }


type GameResult
    = Win
    | Loose


type PlayingGameMsg
    = ColorChanged Color
    | BoardSizeChanged BoardSize


type alias BoardSize =
    Int


type alias Board =
    List Color


type Color
    = Yellow
    | Orange
    | Green
    | Blue
    | Purple
    | Brown


initialBoardSize : BoardSize
initialBoardSize =
    6



------ UPDATE ------


boardSizeToPlayersMoves : BoardSize -> Int
boardSizeToPlayersMoves =
    round << (*) 1.8 << toFloat


generateRandomBoardWithSize : BoardSize -> Random.Generator (List Color)
generateRandomBoardWithSize boardSize =
    Random.list
        (boardSize * boardSize)
        (Random.uniform Yellow [ Orange, Green, Blue, Purple, Brown ])


createNewGame : List Color -> GameStatus
createNewGame colors =
    let
        boardSize =
            round << sqrt << toFloat << List.length <| colors
    in
    GameInProgress
        { board = colors
        , currentColor = withDefault Blue (List.head colors)
        , boardSize = boardSize
        , playerMovesCounter = boardSizeToPlayersMoves boardSize
        }


updateGameStatus : PlayingGameMsg -> GameStatus -> GameStatus
updateGameStatus msg gameStatus =
    case ( msg, gameStatus ) of
        ( ColorChanged newColor, GameInProgress gameState ) ->
            onMove gameState newColor

        ( BoardSizeChanged newSize, GameInProgress gameState ) ->
            GameInProgress
                { gameState
                    | boardSize = newSize
                    , playerMovesCounter = boardSizeToPlayersMoves newSize
                }

        _ ->
            gameStatus


onMove : GameState -> Color -> GameStatus
onMove gameState newColor =
    let
        isWinningMove : Bool
        isWinningMove =
            count ((==) gameState.currentColor) gameState.board
                + count ((==) newColor) gameState.board
                == List.length gameState.board

        nextGameState =
            { gameState
                | board = updateBoard newColor gameState
                , currentColor = newColor
                , playerMovesCounter = gameState.playerMovesCounter - 1
            }
    in
    if newColor == gameState.currentColor then
        GameInProgress gameState

    else if gameState.playerMovesCounter == 1 && not isWinningMove then
        GameFinished nextGameState Loose

    else if isWinningMove then
        GameFinished nextGameState Win

    else
        GameInProgress
            nextGameState


updateBoard : Color -> GameState -> Board
updateBoard newColor { currentColor, boardSize, board } =
    let
        indexedBoard : List ( Int, Color )
        indexedBoard =
            List.indexedMap Tuple.pair board

        oldColorCellsIndexes : List Int
        oldColorCellsIndexes =
            List.map Tuple.first << List.filter (\( _, color ) -> color == currentColor) <| indexedBoard

        isMemberOf : List a -> a -> Bool
        isMemberOf =
            \xs x -> List.member x xs

        indexesToUpdate : List Int
        indexesToUpdate =
            let
                findNeighbours =
                    neighboursToUpdate boardSize oldColorCellsIndexes

                goWith : List Int -> List Int -> List Int
                goWith indexesTillNow indexesToCheck =
                    let
                        uniqueNewNeighboursToUpdate =
                            List.filter (not << isMemberOf indexesTillNow) << unique << List.concatMap findNeighbours <| indexesToCheck
                    in
                    if List.isEmpty uniqueNewNeighboursToUpdate then
                        indexesToCheck

                    else
                        indexesToCheck ++ goWith (indexesTillNow ++ indexesToCheck) uniqueNewNeighboursToUpdate
            in
            goWith [] [ 0 ]

        updateIndexedCells : List ( Int, Color ) -> List ( Int, Color )
        updateIndexedCells =
            updateIf (isMemberOf indexesToUpdate << Tuple.first) ((Tuple.mapSecond << always) newColor)
    in
    (List.map Tuple.second << updateIndexedCells) indexedBoard


neighboursToUpdate : BoardSize -> List Int -> Int -> List Int
neighboursToUpdate boardSize oldColorCellsIndexes currentIndex =
    let
        isLeftmostCell : Bool
        isLeftmostCell =
            remainderBy boardSize currentIndex == 0

        isRightmostCell : Bool
        isRightmostCell =
            remainderBy boardSize currentIndex == boardSize - 1

        isCellFromFirstRow : Bool
        isCellFromFirstRow =
            currentIndex < boardSize

        isCellFromLastRow : Bool
        isCellFromLastRow =
            currentIndex >= boardSize * (boardSize - 1)

        getSameColorNeighbourWhen : Bool -> Int -> List Int
        getSameColorNeighbourWhen predicate neighbour =
            if predicate && List.member neighbour oldColorCellsIndexes then
                [ neighbour ]

            else
                []
    in
    getSameColorNeighbourWhen (not isLeftmostCell) (currentIndex - 1)
        ++ getSameColorNeighbourWhen (not isRightmostCell) (currentIndex + 1)
        ++ getSameColorNeighbourWhen (not isCellFromFirstRow) (currentIndex - boardSize)
        ++ getSameColorNeighbourWhen (not isCellFromLastRow) (currentIndex + boardSize)



------ VIEW ------


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "yellow"

        Orange ->
            "orange"

        Green ->
            "green"

        Blue ->
            "blue"

        Purple ->
            "purple"

        Brown ->
            "brown"
