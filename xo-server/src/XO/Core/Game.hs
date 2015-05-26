module XO.Core.Game where

import XO.Core.Types

import qualified Data.List as DL
import Data.Maybe (Maybe)
import System.Random (StdGen)
import qualified System.Random as SR

move :: StdGen -> Game -> Integer -> Integer -> Integer -> (Game, [Transition], StdGen)
move stdgen game@Game{currentParticipantIndex=currentParticipantIndex} row column playerIndex
    | currentParticipantIndex /= playerIndex = (game, [InvalidInput{description="not your turn"}], stdgen)
    | otherwise = do
        let (game'@Game{winnerParticipantIndex=winnerParticipantIndex}, transitions, stdgen') = move' stdgen game row column playerIndex
        (game', transitions, stdgen')

move' :: StdGen -> Game -> Integer -> Integer -> Integer -> (Game, [Transition], StdGen)
move' stdgen game@Game{currentParticipantIndex=currentParticipantIndex, participants=participants, board=board@Board{tokens=tokens, width=boardWidth}} row column playerIndex =
    do
        let participant = participants!!(fromInteger playerIndex)
        let token = Token column row (ParticipantTokenType playerIndex)
        let board' = board{tokens=(tokens ++ [token])}
        let nextParticipantIndex = (currentParticipantIndex + 1) `mod` (toInteger (DL.length participants))

        let game' = game{board=board'}
        let moveTransition = PlaceToken{
                from = game,
                row = row,
                column = column,
                tokenParticipantIndex = currentParticipantIndex
            }


        -- TODO check for wins
        let (game''@Game{finished=finished}, winTransitions) = checkWin game' boardWidth
        if finished
            then
                (game'', [moveTransition] ++ winTransitions, stdgen)
            else
                do
                    let game''' = game''{currentParticipantIndex=nextParticipantIndex}
                    let startTurnTransition = StartTurn{
                            from = game'',
                            newParticipantIndex=nextParticipantIndex
                        }
                    let moveTransitions = [moveTransition, startTurnTransition]
                    let (game'''', aiTransitions, stdgen') = aimove stdgen game'''
                    (game'''', moveTransitions ++ winTransitions ++ aiTransitions, stdgen')

checkWin :: Game -> Integer -> (Game, [Transition])
checkWin game@Game{board=Board{width=boardWidth, height=boardHeight, tokens=tokens}, currentParticipantIndex=currentParticipantIndex} winLength =
    do
        if DL.length tokens == fromInteger (boardWidth * boardHeight)
            then
                do
                    let finishTransition = DrawGame{from=game}
                    let game' = game{finished=True}
                    (game', [finishTransition])

            else
                do
                    let points = [(x, y) | x <- [0..(boardWidth-1)], y<- [0..(boardHeight-1)]]
                    let win = DL.foldl (\acc point -> acc || (checkWin' game winLength point)) False points
                    if win
                        then
                            do
                                let winTransition = WinGame{from=game, winnerIndex=currentParticipantIndex}
                                let game' = game{winnerParticipantIndex=Just currentParticipantIndex, finished=True}
                                (game', [winTransition])
                        else (game, [])

checkWin' :: Game -> Integer -> (Integer, Integer) -> Bool
checkWin' game@Game{board=board} winLength p@(x, y) =
    do
        let maybeToken = getToken board x y
        case maybeToken of
            Nothing -> False
            Just token@Token{tokenType=tokenType} ->
                do
                    let maybeParticipantIndex = (\tokenType -> do
                        case tokenType of
                            ParticipantTokenType participantIndex -> Just participantIndex
                            _ -> Nothing) tokenType
                    (checkWin'' board winLength p (0, 1) maybeParticipantIndex) || (checkWin'' board winLength p (1, 0) maybeParticipantIndex) || (checkWin'' board winLength p (1, 1) maybeParticipantIndex)

checkWin'' :: Board -> Integer -> (Integer, Integer) -> (Integer, Integer) -> Maybe Integer -> Bool
checkWin'' _ 0 _ _ _ = True
checkWin'' board winLength (x, y) (dx, dy) (Just index) =
    do
        let maybeToken = getToken board x y
        case maybeToken of
            Nothing -> False
            (Just token@Token{tokenType=tokenType}) ->
                do
                    case tokenType of
                        ParticipantTokenType participantIndex ->
                            if participantIndex == index
                                then checkWin'' board (winLength - 1) (x+dx, y+dy) (dx, dy) (Just index)
                                else False
                        _ -> False

aimove :: StdGen -> Game -> (Game, [Transition], StdGen)
aimove stdgen game@Game{currentParticipantIndex=playerIndex, participants=participants, board=board@Board{width=boardWidth, height=boardHeight, tokens=tokens}} =
    do
        let participant = participants!!(fromInteger playerIndex)
        case participant of
            PlayerParticipant _ -> (game, [], stdgen)
            RandomAIParticipant ->
                do
                    let spaces = boardWidth * boardHeight - (toInteger $ DL.length tokens)
                    let (n, stdgen') = SR.next stdgen
                    let space = (toInteger n) `mod` spaces
                    let (x, y) = findSpot board space
                    move' stdgen' game y x playerIndex


findSpot :: Board -> Integer -> (Integer, Integer)
findSpot board space = findSpot' board space 0

findSpot' :: Board -> Integer -> Integer -> (Integer, Integer)
findSpot' board@Board{width=boardWidth} space progress =
    do
        let x = progress `mod` boardWidth
        let y = progress `quot` boardWidth
        let maybeToken = getToken board x y
        case maybeToken of
            Nothing ->
                if space == 0
                    then (x, y)
                    else findSpot' board (space-1) (progress+1)
            Just _ ->  findSpot' board space (progress+1)

getToken :: Board -> Integer -> Integer -> Maybe Token
getToken board@Board{tokens=tokens} x y = DL.find (\token@Token{y=row, x=column} -> row == y && column == x) tokens

