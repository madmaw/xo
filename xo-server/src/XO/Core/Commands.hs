module XO.Core.Commands(
    Row,
    Column,
    Command(..),
    GameLookup(..)
) where

import Data.Maybe (Maybe)
import XO.Core.Types (GameId, PlayerId, PlayerDetails, MessageId)

type Row = Integer
type Column = Integer

data GameLookup = GameLookup {
    lookupMessageId :: Maybe MessageId,
    lookupPlayerIds :: [PlayerId]
} deriving (Show, Read, Eq)

data Command =
    Move PlayerId GameLookup Row Column |
    Quit PlayerId GameLookup |
    Redraw GameLookup |
    Register PlayerDetails |
    Unregister PlayerId |
    Challenge MessageId PlayerDetails [PlayerDetails] |
    Update PlayerDetails |
    InvalidCommand {
        source :: Maybe PlayerId,
        game :: Maybe GameLookup,
        input :: String,
        reason :: String
    } deriving (Show, Read, Eq)

