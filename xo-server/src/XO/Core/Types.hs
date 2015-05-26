module XO.Core.Types where

import Data.Maybe (Maybe)

type GameId = Integer
type MessageId = Integer

data Game =
    Game {
        gameId :: GameId,
        messageIds :: [MessageId],
        participants :: [Participant],
        currentParticipantIndex :: Integer,
        board :: Board,
        winnerParticipantIndex :: Maybe Integer,
        finished :: Bool
    } deriving (Show, Read)

data Board =
    Board {
        width :: Integer,
        height :: Integer,
        tokens :: [Token]
    } deriving (Show, Read)

data Token =
    Token {
        x :: Integer,
        y :: Integer,
        tokenType :: TokenType
    } deriving (Show, Read)

data TokenType =
    ParticipantTokenType {
        participantIndex :: Integer
    } |
    WildcardTokenType deriving (Show, Read)

data Participant =
    PlayerParticipant PlayerId |
    RandomAIParticipant deriving (Show, Read)

type PlayerId = Integer

data Player =
    Player {
        playerDetails :: PlayerDetails,
        activeGames :: [GameId],
        wonGames :: Integer,
        totalGames :: Integer,
        registered :: Bool
    } deriving (Show, Read)

data PlayerDetails =
    PlayerDetails {
        playerId :: PlayerId,
        imageURL :: Maybe String,
        displayName :: String
    } deriving (Show, Read, Eq)

data GameDelta =
    GameDelta {
        target :: Either PlayerId GameId,
        transitions :: [Transition]
    } deriving (Show, Read)

data Transition =
    PlaceToken {
        from :: Game,
        row :: Integer,
        column :: Integer,
        tokenParticipantIndex :: Integer
    } |
    StartGame |
    Noop |
    StartTurn {
        from :: Game,
        newParticipantIndex :: Integer
    } |
    WinGame {
        from :: Game,
        winnerIndex :: Integer
    } |
    DrawGame {
        from :: Game
    } |
    Refresh String |
    InvalidInput {
        description :: String
    } deriving (Show, Read)
