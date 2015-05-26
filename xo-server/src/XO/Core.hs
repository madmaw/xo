module XO.Core where

import XO.Core.Game
import XO.Core.Commands
import XO.Core.Types

import qualified Data.List as DL
import Data.Map (Map, insert)
import qualified Data.Map as DM
import qualified Data.Tree as DT
import Data.Maybe (Maybe)
import Data.Either (Either)
import Control.Monad (liftM, liftM2)
import System.Random (StdGen)

data World =
    World {
        stdgen :: StdGen,
        outstandingCommands::[Command],
        messageIdsToGameIds :: Map MessageId GameId,
        games :: Map GameId Game,
        players :: Map PlayerId Player
    } deriving (Show, Read)

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe Nothing Nothing = Nothing
mergeMaybe ja@(Just a) Nothing = ja
mergeMaybe Nothing ja@(Just a) = ja
mergeMaybe (Just a) jb@(Just b) = jb

mergePlayerDetailsIntoPlayer :: PlayerDetails -> Maybe Player -> Player
mergePlayerDetailsIntoPlayer newPlayerDetails Nothing = Player{
        playerDetails = newPlayerDetails,
        activeGames = [],
        wonGames = 0,
        totalGames = 0,
        registered = False
    }
mergePlayerDetailsIntoPlayer
        PlayerDetails{displayName=newDisplayName, imageURL=newImageURL}
        (Just player@Player{playerDetails=oldPlayerDetails@PlayerDetails{imageURL=oldImageURL}})=
    do
        let mergedPlayerDetails = oldPlayerDetails{
                imageURL = mergeMaybe oldImageURL newImageURL,
                displayName = newDisplayName
            }
        player{playerDetails = mergedPlayerDetails}

mergePlayerDetailsIntoWorld :: World -> PlayerDetails -> World
mergePlayerDetailsIntoWorld world@World{players=players} playerDetails@PlayerDetails{playerId=playerId} =
    do
        let maybePlayer = DM.lookup playerId players
        let player = mergePlayerDetailsIntoPlayer playerDetails maybePlayer
        world{players = (insert playerId player players)}

applyCommand :: World -> Command -> World
applyCommand world (Update playerDetails) = mergePlayerDetailsIntoWorld world playerDetails

applyCommand world@World{players=players} (Register playerDetails@PlayerDetails{playerId=playerId}) =
    do
        let maybePlayer = DM.lookup playerId players
        case maybePlayer of
            Nothing ->
                do
                    let player = Player{
                            playerDetails=playerDetails,
                            activeGames = [],
                            wonGames = 0,
                            totalGames = 0,
                            registered = True
                        }
                    world{players = (insert playerId player players)}
            (Just player) -> world{players = (insert playerId player{registered=True} players)}

applyCommand world@World{players=players} (Unregister playerId) =
    do
        let maybePlayer = DM.lookup playerId players
        case maybePlayer of
            Nothing -> world
            (Just player) -> world{players = (insert playerId player{registered=False} players)}
-- TODO remove any duplicates
applyCommand world@World{outstandingCommands=outstandingCommands} command =
    do
        world{outstandingCommands = outstandingCommands ++ [command]}

update :: World -> (World, Maybe GameDelta)
update world@World{outstandingCommands=[]} = (world, Nothing)
update world@World{outstandingCommands=(command:otherCommands)} =
    do
        let (world', gameDelta) = updateWithCommand world command
        let world'' = world'{outstandingCommands = otherCommands}
        (world'', gameDelta)

updateWithCommand :: World -> Command -> (World, Maybe GameDelta)
updateWithCommand world InvalidCommand{reason=reason, source=source, game=game} =
    do
        let target = toTarget world source game
        let transition = InvalidInput {
                description = reason
            }
        let gameDelta = liftM2 GameDelta target (Just [transition])
        -- want to add the message id to the game ids, even if we don't respond
        let world' = addGameLookupMessageIdToWorld world target game
        (world', gameDelta)

updateWithCommand world@World{stdgen=stdgen} (Challenge messageId challengerPlayerDetails challengedPlayersDetails) =
    do
        let allPlayerDetails = challengerPlayerDetails:challengedPlayersDetails
        let allPlayerIds = map (\PlayerDetails{playerId = playerId} -> playerId) allPlayerDetails
        -- merge in the player details
        let world' = foldl mergePlayerDetailsIntoWorld world allPlayerDetails
        -- check that a game between these players doesn't already exist
        let existingGameId = searchTargetGameId world' allPlayerIds
        case existingGameId of
            Nothing ->
                do
                    -- create a game
                    let participants = createParticipants world' allPlayerIds 2
                    let board = Board {
                            width=3,
                            height=3,
                            tokens=[]
                        }
                    let starterIndex = 0
                    let game = Game {
                            winnerParticipantIndex=Nothing,
                            gameId=messageId,
                            messageIds=[messageId],
                            participants=participants,
                            currentParticipantIndex=starterIndex,
                            board=board,
                            finished=False
                        }
                    let (game', automaticTransitions, stdgen') = aimove stdgen game
                    let world'' = addGame world'{stdgen=stdgen'} game'
                    let transitions = [StartGame, StartTurn game starterIndex] ++ automaticTransitions
                    let gameDelta = GameDelta{
                            target = Right messageId,
                            transitions = transitions
                        }
                    (world'', Just gameDelta)
            Just gameId ->
                do
                    -- redraw that game
                    let transition = Refresh "Game already in progress!"
                    let gameDelta = GameDelta (Right gameId) [transition]
                    let world'' = addMessageIdToWorld world gameId messageId
                    (world'', Just gameDelta)
updateWithCommand world@World{stdgen=stdgen, games=games, messageIdsToGameIds=messageIdsToGameIds} (Move playerId gameLookup row column) =
    do
        let target = toTarget world (Just playerId) (Just gameLookup)
        case target of
            Just (Right gameId) ->
                do
                    let world' = addGameLookupMessageIdToWorld world target (Just gameLookup)
                    let (Just game@Game{board=Board{tokens=tokens}, participants=participants, messageIds=messageIds}) = DM.lookup gameId games
                    let maybePlayerIndex = findPlayerIndex participants playerId
                    case maybePlayerIndex of
                        Just playerIndex ->
                            do
                                let maybeToken = findToken tokens row column
                                case maybeToken of
                                    Nothing ->
                                        do
                                            let (game', transitions, stdgen') = move stdgen game row column playerIndex
                                            let world'' = world{games=DM.insert gameId game' games}
                                            let gameDelta = GameDelta (Right gameId) transitions
                                            (world''{stdgen=stdgen'}, Just gameDelta)

                                    Just token ->
                                        do
                                            let transition = InvalidInput {
                                                    description = "Already a token there"
                                                }
                                            let gameDelta = GameDelta (Right gameId) [transition]
                                            (world', Just gameDelta)
                        Nothing ->
                            do
                                let transition = InvalidInput {
                                        description = "You're not playing this game"
                                    }
                                let gameDelta = GameDelta (Left playerId) [transition]
                                (world', Just gameDelta)


            _ ->
                do
                    let transition = InvalidInput {
                            description = "Not playing a game"
                        }
                    let gameDelta = GameDelta (Left playerId) [transition]

                    (world, Just gameDelta)

updateWithCommand world _ = (world, Nothing)

findToken :: [Token] -> Integer -> Integer -> Maybe Token
findToken [] row column = Nothing
findToken (token@Token{x=x, y=y}:xs) row column
    | x == column && y == row = Just token
    | otherwise = findToken xs row column

findPlayerIndex :: [Participant] -> PlayerId -> Maybe Integer
findPlayerIndex = findPlayerIndex' 0

findPlayerIndex' :: Integer -> [Participant] -> PlayerId -> Maybe Integer
findPlayerIndex' _ [] _ = Nothing
findPlayerIndex' index ((PlayerParticipant participantPlayerId):xs) playerId
    | playerId == participantPlayerId = Just index
    | otherwise = findPlayerIndex' (index+1) xs playerId
findPlayerIndex' index (_:xs) playerId = findPlayerIndex' (index+1) xs playerId


addGame :: World -> Game -> World
addGame world@World{games=games} game@Game{gameId=gameId,messageIds=messageIds} =
    do
        let world' = world{games=insert gameId game games}
        foldl (\world@World{messageIdsToGameIds=messageIdsToGameIds} messageId -> world{messageIdsToGameIds=insert messageId gameId messageIdsToGameIds}) world' messageIds


createParticipants :: World -> [PlayerId] -> Integer -> [Participant]
createParticipants world [] n
    | n > 0     = (createParticipants world [] (n-1)) ++ [RandomAIParticipant]
    | otherwise = []
createParticipants world (playerId:otherPlayerIds) n = (createParticipants world otherPlayerIds (n-1)) ++ [PlayerParticipant playerId]

addGameLookupMessageIdToWorld :: World -> Maybe (Either PlayerId GameId) -> Maybe GameLookup -> World
addGameLookupMessageIdToWorld world (Just (Right gameId)) (Just GameLookup{lookupMessageId=(Just messageId)}) =
    addMessageIdToWorld world gameId messageId
addGameLookupMessageIdToWorld world _ _ = world

addMessageIdToWorld :: World -> GameId -> MessageId -> World
addMessageIdToWorld world@World{messageIdsToGameIds=messageIdsToGameIds} gameId messageId =
    world{messageIdsToGameIds = insert messageId gameId messageIdsToGameIds}


toTarget :: World -> Maybe PlayerId -> Maybe GameLookup -> Maybe (Either PlayerId GameId)
toTarget _ Nothing Nothing = Nothing
toTarget _ (Just playerId) Nothing = Just $ Left playerId
toTarget world maybePlayerId (Just gameLookup) =
    do
        let maybeGameId = toTargetGameId world gameLookup
        case maybeGameId of
            Nothing ->
                liftM Left maybePlayerId
            Just gameId ->
                Just $ Right gameId

toTargetGameId :: World -> GameLookup -> Maybe GameId
toTargetGameId World{messageIdsToGameIds=messageIdsToGameIds} GameLookup{lookupMessageId=Just messageId, lookupPlayerIds=playerIds} = DM.lookup messageId messageIdsToGameIds
toTargetGameId world GameLookup{lookupMessageId=Nothing, lookupPlayerIds=playerIds} = searchTargetGameId world playerIds

searchTargetGameId :: World -> [PlayerId] -> Maybe GameId
-- TODO implement search
searchTargetGameId _ _ = Nothing


participantDisplayName :: World -> Game -> Integer -> String
participantDisplayName world@World{players=players} game@Game{participants=participants} participantIndex =
    do
        let participant = participants!!(fromInteger participantIndex)
        case participant of
            PlayerParticipant playerId ->
                do
                    let (Just Player{playerDetails=PlayerDetails{displayName=displayName}}) = DM.lookup playerId players
                    displayName
            _ -> "Player "++(show $ participantIndex + 1)
