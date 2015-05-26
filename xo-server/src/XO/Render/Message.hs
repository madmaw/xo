module XO.Render.Message where

import XO.Render
import XO.Core
import XO.Core.Types

import qualified Data.Map as DM
import qualified Data.List as DL

renderToMessage :: World -> GameId -> [Transition] -> String
renderToMessage world gameId transitions = DL.foldl (accumulateTransition world) "" transitions

accumulateTransition :: World -> String -> Transition -> String
accumulateTransition world@World{players=players} oldMessage InvalidInput{description=description} = description
accumulateTransition world oldMessage StartTurn{newParticipantIndex=newParticipantIndex, from=game} =
    do
        let displayName = participantDisplayName world game newParticipantIndex
        displayName ++ "'s turn"
accumulateTransition _ oldMessage _ = oldMessage
