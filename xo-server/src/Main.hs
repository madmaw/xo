{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import XO.Core
import XO.Core.Types
import XO.Core.Commands
import XO.Conduit.Stdin
import XO.Conduit.Twitter
import XO.Render
import XO.Render.SVG
import XO.Render.SVGCairo
import XO.Render.AnimatedGIF
import XO.Render.Message

import qualified Data.String.Conversions as DSC

import qualified Data.ByteString as DBS
import qualified Data.ByteString.Lazy as DBSLZ
import qualified Data.Binary as DB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CTM
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Control.Monad.Trans.Resource as RT

import qualified Web.Twitter.Types as WTT
import qualified Web.Twitter.Conduit as WTC

import qualified Web.Authenticate.OAuth as OAuth

import qualified Network.HTTP.Conduit as NHC
import qualified Data.String.Conversions as DSC
import qualified Data.ByteString.Char8 as B8

import Control.Monad (liftM, liftM3, join)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
import Debug.Trace (trace)
import Data.Default (def)
import Data.Either (Either)
import Data.Map (empty, mapWithKey)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist)
import System.CPUTime (getCPUTime)
import System.Random (StdGen)
import qualified System.Random as SR

credentialFilePath = ".accessToken"


twitterOAuth :: OAuth.OAuth
twitterOAuth = WTC.twitterOAuth
    { OAuth.oauthConsumerKey = "lDzqm4qaYpGCxITCUgesr16TV"
    , OAuth.oauthConsumerSecret = "zFJSGsA9tDNmTbxaZL7EqSvDgOIknq7svg22dVt3Klphs6f6Cl"
    }


authorize :: (RT.MonadBaseControl IO m, RT.MonadResource m)
          => OAuth.OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> NHC.Manager
          -> m OAuth.Credential
authorize oauth getPIN mgr =
    do
        exists <- liftIO $ doesFileExist credentialFilePath
        if exists
            then do
                storedToken <- liftIO $ DB.decodeFileOrFail credentialFilePath
                case storedToken of
                    Left _ -> authorizeAndWriteOut oauth getPIN mgr
                    Right tokens ->
                        return $ OAuth.inserts tokens OAuth.emptyCredential
            else authorizeAndWriteOut oauth getPIN mgr


authorizeAndWriteOut :: (RT.MonadBaseControl IO m, RT.MonadResource m)
          => OAuth.OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> NHC.Manager
          -> m OAuth.Credential
authorizeAndWriteOut oauth getPIN mgr =
    do
        tempCred <- OAuth.getTemporaryCredential oauth mgr
        let url = OAuth.authorizeUrl oauth tempCred
        pin <- getPIN url
        cred <- OAuth.getAccessToken oauth (OAuth.insert "oauth_verifier" (B8.pack pin) tempCred) mgr
        liftIO $ DB.encodeFile credentialFilePath $ OAuth.unCredential cred
        return cred


getTWInfo :: NHC.Manager -> (RT.ResourceT IO) WTC.TWInfo
getTWInfo mgr =
    do
        cred <- authorize twitterOAuth getPIN mgr
        return $ WTC.setCredential twitterOAuth cred def
    where
        getPIN url = liftIO $ do
            putStrLn $ "browse URL: "++url
            putStrLn $ ">what was the PIN twitter provided? "
            hFlush stdout
            getLine

eatCommand :: (MVar World) -> Command -> (RT.ResourceT IO) ()
eatCommand worldRef command =
    liftIO $ do
        world <- takeMVar worldRef
        let world' = applyCommand world command
        putMVar worldRef world'
        putStrLn $ show world'

commandSink :: (MVar World) -> C.Sink Command (RT.ResourceT IO) () -- consumes a stream of Show-instances, no result
commandSink worldRef = CL.mapM_ $ eatCommand worldRef

checkUnfollowThread :: (MVar World) -> WTC.TWInfo -> NHC.Manager -> (RT.ResourceT IO) ()
checkUnfollowThread worldRef twInfo mgr =
    do
        startTime <- liftIO $ getCPUTime
        let source = loadFollowers twInfo mgr
        followerIds <- source C.$$ CL.consume
        -- do a diff on the registered users
        world <- liftIO $ takeMVar worldRef
        let playerMap = players world
        let playerMap' = mapWithKey (\playerId player@Player{registered=wasRegistered} -> do
            let isRegistered = elem playerId followerIds
            case () of
                _ | isRegistered == wasRegistered -> player
                  | otherwise                     -> player{registered=isRegistered}) playerMap

        let world' = world{players = playerMap'}
        liftIO $ putMVar worldRef world'
        -- sleep and repeat
        -- factor in the execution time of the above code
        endTime <- liftIO $ getCPUTime
        -- picoseconds to microseconds
        let waitTime = 90000000 - ((endTime - startTime) `quot` 1000000)
        liftIO $ putStrLn $ "followers "++ (show followerIds) ++ "  next check in " ++ (show (waitTime `quot` 1000000)) ++ " s"
        if waitTime > 0
            then liftIO $ threadDelay $ fromIntegral waitTime
            else return ()
        checkUnfollowThread worldRef twInfo mgr


annotateMessage :: String -> World -> GameId -> String
annotateMessage message world@World{games=games} gameId =
    do
        let (Just Game{participants=participants}) = DM.lookup gameId games
        let replies = DL.foldl (toReply world) "" participants
        replies ++ message

toReply :: World -> String -> Participant -> String
toReply world@World{players=players} acc (PlayerParticipant playerId) =
    do
        let (Just Player{playerDetails=PlayerDetails{displayName=displayName}}) = DM.lookup playerId players
        acc ++ "@" ++ displayName ++ " "
toReply _ acc _ = acc

postResponseThread :: (MVar World) -> WTC.TWInfo -> NHC.Manager -> (RT.ResourceT IO) ()
postResponseThread worldRef twInfo mgr =
    do
        startTime <- liftIO $ getCPUTime
        world <- liftIO $ takeMVar worldRef
        let (world', maybeDelta) = update world
        liftIO $ putStrLn $ "updated "++ (show maybeDelta)
        wait <- do
            case maybeDelta of
                Just delta@GameDelta{target=Right gameId, transitions=transitions} ->
                    do
                        image <- liftIO $ renderToAnimatedGIF world' gameId transitions
                        let message = renderToMessage world' gameId transitions
                        let message' = annotateMessage message world' gameId

                        maybeImageData <- do
                            case image of
                                Left errorMessage ->
                                    do
                                        liftIO $ putStrLn errorMessage
                                        return Nothing
                                Right imageData -> return $ Just imageData
                        response <- submit mgr twInfo (Just message') maybeImageData
                        let world'' = applyResponse world' delta response
                        let world''' = removeFinishedGame world'' gameId
                        liftIO $ putMVar worldRef world'''
                        return True
                Nothing ->
                    do
                        liftIO $ putMVar worldRef world'
                        return False
        endTime <- liftIO $ getCPUTime
        -- TODO wait for input if we're out of input
        if wait
            then
                do
                    -- picoseconds to microseconds
                    let waitTime = 30000000 - ((endTime - startTime) `quot` 1000000)
                    -- TODO if the outstanding commands were empty, wait on the commands rather than the time
                    if waitTime > 0
                        then liftIO $ threadDelay $ fromIntegral waitTime
                        else return ()
            else liftIO $ threadDelay 1000000 -- just wait a little while
        -- does this actually wait?
        postResponseThread worldRef twInfo mgr

removeFinishedGame :: World -> GameId -> World
removeFinishedGame world@World{games=games, messageIdsToGameIds=messageIdsToGameIds} gameId =
    do
        let maybeGame = DM.lookup gameId games
        case maybeGame of
            Just game@Game{messageIds=messageIds, finished=finished} ->
                if finished
                    then world{games=DM.delete gameId games, messageIdsToGameIds = DL.foldl (\m mid -> DM.delete mid m) messageIdsToGameIds messageIds}
                    else world
            Nothing ->
                world

main :: IO ()
main =
    do
        -- TODO load from file
        -- TODO backfill any missed tweets
        stdgen <- SR.newStdGen
        let initialWorld = World {
            stdgen = stdgen,
            outstandingCommands = [],
            messageIdsToGameIds = empty,
            games = empty,
            players = empty
        }
        -- the only mutable thing in our app
        let worldRef = unsafePerformIO (newMVar initialWorld)


        NHC.withManager $ \mgr ->
            do
                twInfo <- getTWInfo mgr
                -- listen for unfollows
                RT.resourceForkIO $ checkUnfollowThread worldRef twInfo mgr
                RT.resourceForkIO $ postResponseThread worldRef twInfo mgr

                liftIO $ putStrLn $ "got " ++ (show twInfo)
                twitterR <- commandTwitterSource twInfo mgr
                -- TODO what do we do with the finalizer?
                (twitter, twitterFinalizer) <- C.unwrapResumable twitterR
                src <- commandStdinSource CTM.>=< twitter
                src C.$$ (commandSink worldRef)

