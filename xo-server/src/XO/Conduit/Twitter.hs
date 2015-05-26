{-# LANGUAGE OverloadedStrings #-}

module XO.Conduit.Twitter where

import XO.Core
import XO.Core.Commands
import XO.Core.Types
import XO.Render

import qualified Data.ByteString.Lazy as DBSLZ
import qualified Data.String.Conversions as DSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Trans.Resource as RT

import qualified Web.Twitter.Types as WTT
import qualified Web.Twitter.Conduit as WTC

import qualified Web.Authenticate.OAuth as OAuth

import qualified Network.HTTP.Conduit as NHC

import Data.ByteString (ByteString)
import Data.Char (toLower, ord, isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (insert)
import Debug.Trace (trace)
import Control.Monad (liftM, liftM3)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Maybe (Maybe)
import System.IO (stdin)
import Data.Either (Either)

streamingAPIToCommandConduit :: RT.MonadResource m => WTT.UserId -> C.Conduit WTT.StreamingAPI m Command
streamingAPIToCommandConduit userId = C.awaitForever (\s ->
        do
            let commands = filterMaybes $ convertStreamingAPIToCommands userId s
            CL.sourceList commands
    )

filterMaybes :: [Maybe a] -> [a]
filterMaybes [] = []
filterMaybes (Just a : xs) = a : filterMaybes xs
filterMaybes (Nothing : xs) = filterMaybes xs


-- TODO unfollows?!

trimMentions :: String -> [String]
trimMentions s = do
    let words = splitOn " " s
    filterMentions words

filterMentions :: [String] -> [String]
filterMentions [] = []
filterMentions (('@':_):xs) = filterMentions xs
filterMentions (x:xs) =
    do
        let isWhitespace = foldl (\b c -> b && isSpace c) True x
        if isWhitespace
            then filterMentions xs
            else (map toLower x) : filterMentions xs

twitterUserToPlayerDetails :: WTT.User -> Maybe PlayerDetails
twitterUserToPlayerDetails WTT.User {
        WTT.userId = userId,
        WTT.userProfileImageURL = Just userProfileImageURL,
        WTT.userScreenName = userScreenName
    } = Just PlayerDetails {
            playerId = userId,
            imageURL = Just $ DSC.cs userProfileImageURL,
            displayName = DSC.cs userScreenName
        }

convertEntitiesToPlayerDetails :: Maybe WTT.Entities -> [PlayerDetails]
convertEntitiesToPlayerDetails Nothing = []
convertEntitiesToPlayerDetails (Just WTT.Entities {WTT.enUserMentions=userMentions}) = filterMaybes $ map twitterUserEntityToPlayerDetails userMentions

twitterUserEntityToPlayerDetails :: WTT.Entity WTT.UserEntity -> Maybe PlayerDetails
twitterUserEntityToPlayerDetails WTT.Entity {
        WTT.entityBody = WTT.UserEntity {
            WTT.userEntityUserId = userId,
            WTT.userEntityUserScreenName = userScreenName
        }
    } = Just PlayerDetails {
            playerId = userId,
            imageURL = Nothing,
            displayName = DSC.cs userScreenName
        }


charsToMove :: WTT.UserId -> GameLookup -> Char -> Char -> Maybe Command
charsToMove userId gameLookup rowChar colChar = do
    let lowerRowChar = toLower rowChar
    let lowerColChar = toLower colChar
    charsToMove' userId gameLookup lowerRowChar lowerColChar

charsToMove' :: WTT.UserId -> GameLookup -> Char -> Char -> Maybe Command
charsToMove' userId gameLookup rowChar colChar | rowCharSwitched && colCharSwitched = charsToMove' userId gameLookup colChar rowChar
                                               | rowCharValid && colCharValid =  Just $ Move userId gameLookup (toInteger (ord rowChar - ord '1')) (toInteger (ord colChar - ord 'a'))
                                               | rowCharValid = Just $ InvalidCommand (Just userId) (Just gameLookup) ([rowChar] ++ " " ++ [colChar]) ("invalid column "++[colChar])
                                               | colCharValid = Just $ InvalidCommand (Just userId) (Just gameLookup) ([rowChar] ++ " " ++ [colChar]) ("invalid row "++[rowChar])
                                               | otherwise = Just $ InvalidCommand (Just userId) (Just gameLookup) ([rowChar] ++ " " ++ [colChar]) ("invalid row "++[rowChar]++" and column "++[colChar])
    where
        rowCharValid = rowChar >= '0' && rowChar <= '9'
        colCharValid = colChar >= 'a' && colChar <= 'j'
        rowCharSwitched = rowChar >= 'a' && rowChar <= 'j'
        colCharSwitched = colChar >= '0' && colChar <= '9'


convertStreamingAPIToCommands :: WTT.UserId -> WTT.StreamingAPI -> [Maybe Command]
convertStreamingAPIToCommands ownerUserId (WTT.SStatus WTT.Status {
        WTT.statusId = statusId,
        WTT.statusText = statusText,
        WTT.statusInReplyToStatusId = maybeGameId,
        WTT.statusUser = user,
        WTT.statusEntities = statusEntities
    }) =
        do
            -- TODO have a case without an update when the user isn't populated
            let (Just senderPlayerDetails) = twitterUserToPlayerDetails user
            let senderPlayerId = playerId senderPlayerDetails
            if senderPlayerId /= ownerUserId
                then
                    do
                        let update = Just $ Update senderPlayerDetails
                        -- trim out the mentions in the update
                        let instructionWords = trimMentions $ DSC.cs statusText
                        -- TODO make sure the challenger isn't challenging themselves
                        let mentionedPlayerDetails = filter (\PlayerDetails{playerId=playerId} -> playerId /= ownerUserId ) $ convertEntitiesToPlayerDetails statusEntities
                        let gameLookup = GameLookup {
                            lookupMessageId=maybeGameId,
                            lookupPlayerIds = map (\PlayerDetails{playerId=playerId} -> playerId) mentionedPlayerDetails
                        }
                        case instructionWords of
                            [(rowChar:(colChar:[]))] -> do
                                let instruction = charsToMove (playerId senderPlayerDetails) gameLookup rowChar colChar
                                [update, instruction]
                            [(rowChar:[]), (colChar:[])] -> do
                                let instruction = charsToMove (playerId senderPlayerDetails) gameLookup rowChar colChar
                                [update, instruction]
                            [] -> do
                                let instruction = Just $ Challenge statusId senderPlayerDetails mentionedPlayerDetails
                                [instruction]
                            ["quit"] -> do
                                let instruction = Just $ Quit (playerId senderPlayerDetails) gameLookup
                                [instruction]
                            _ -> do
                                let instruction = Just $ InvalidCommand {
                                    source = Nothing,
                                    game = Just gameLookup,
                                    input = DSC.cs statusText,
                                    reason = "invalid instruction " ++ (intercalate " " instructionWords)
                                }
                                [update, instruction]
                else []

convertStreamingAPIToCommands ownerUserId (WTT.SEvent e@WTT.Event{
            WTT.evEvent = event,
            WTT.evSource = WTT.ETUser sourceUser,
            WTT.evTarget = WTT.ETUser WTT.User {
                WTT.userId = targetUserId
            }
        }) =
    do
        let sourcePlayer = twitterUserToPlayerDetails sourceUser
        case event of
            "follow" ->
                if targetUserId == ownerUserId
                    then [liftM Register sourcePlayer]
                    else []
            _ ->
                [
                    Just $ InvalidCommand {
                        source = liftM playerId sourcePlayer,
                        game = Nothing,
                        input = show e,
                        reason = "unknown event type " ++ (DSC.cs event)
                    }
                ]
convertStreamingAPIToCommands _ a = [
        Just $ InvalidCommand {
            source = Nothing,
            game = Nothing,
            input = show a,
            reason = "fucks given: 0"
        }
    ]

getUserId :: WTC.TWInfo -> WTT.UserId
getUserId WTC.TWInfo {
            WTC.twToken = WTC.TWToken {
                WTC.twCredential = OAuth.Credential {
                    OAuth.unCredential = tokens
                }
            }
        } =
    do
        let Just userIdString = lookup "user_id" tokens
        read $ DSC.cs userIdString

commandTwitterSource :: RT.MonadResource m => WTC.TWInfo -> NHC.Manager -> m (C.ResumableSource m Command)
commandTwitterSource twInfo mgr =
    do
        let userId = getUserId twInfo

        src <- WTC.stream twInfo mgr WTC.userstream
        return $ src C.$=+ streamingAPIToCommandConduit userId

loadFollowers :: RT.MonadResource m => WTC.TWInfo -> NHC.Manager -> C.Source m WTT.UserId
loadFollowers twInfo mgr =
    do
        let userId = WTC.UserIdParam $ getUserId twInfo
        WTC.sourceWithCursor twInfo mgr (WTC.followersIds userId)



submit :: MonadResource m => NHC.Manager -> WTC.TWInfo -> Maybe String -> Maybe DBSLZ.ByteString -> m WTT.Status
submit mgr twInfo (Just message) Nothing =
    do
        WTC.call twInfo mgr $ WTC.update $ DSC.cs message
submit mgr twInfo (Just message) (Just image) =
    do
        let media = WTC.MediaRequestBody "x" $ NHC.RequestBodyLBS image
        WTC.call twInfo mgr $ WTC.updateWithMedia (DSC.cs message) media

-- TODO reply to the correct message
-- TODO reply to all the players
-- TODO convert any frames to animated gif

applyResponse :: World -> GameDelta -> WTT.Status -> World
applyResponse world@World{messageIdsToGameIds=messageIdsToGameIds} GameDelta{target=Right gameId} WTT.Status{WTT.statusId=statusId} =
    world{messageIdsToGameIds=insert statusId gameId messageIdsToGameIds}
applyResponse world _ _ = world
