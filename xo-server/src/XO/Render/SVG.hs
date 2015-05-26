{-# LANGUAGE OverloadedStrings #-}


module XO.Render.SVG where

import XO.Render
import XO.Core
import XO.Core.Types

import qualified Codec.Picture as CP

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze as T
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Text.Blaze.Svg.Renderer.String as SS

import qualified System.Directory as Dir
import qualified Data.Char as DC
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Lazy as DBSLZ
import qualified Data.ByteString.Base64 as DBS64
import qualified Data.ByteString.Char8 as DBSC
import qualified Data.String.Conversions as DSC
import qualified Data.Map as DM
import qualified Data.Traversable as DT
import qualified Data.List as DL
import qualified Network.Curl.Download as NCD
import qualified Network.Curl.Opts as NCO
import Control.Monad.IO.Class (liftIO)

renderToSVG :: World -> GameId -> [Transition] -> IO [Frame String]
renderToSVG world gameId transitions@(x:xs) =
    do
        -- prefix (very briefly) with the final frame and make the final frame last ages
        frames <- renderToSVG' world gameId transitions
        if DL.length frames > 0
            then
                do
                    let (Frame _ svg) = DL.last frames
                    return $ [Frame 1 svg] ++ (DL.take ((DL.length frames) - 1) frames) ++ [Frame 5000 svg]
            else renderToSVG world gameId []

renderToSVG world@World{games=games} gameId [] =
    do
        let maybeGame = DM.lookup gameId games
        case maybeGame of
            Just game ->
                do
                    svg <- renderGame world game Nothing
                    let boardSVG = SS.renderSvg svg
                    return [Frame 5000 boardSVG]
            Nothing ->
                return []


renderToSVG' :: World -> GameId -> [Transition] -> IO [Frame String]
renderToSVG' world gameId ((transition@WinGame{from=game}):xs) =
    do
        frames <- animateFrames world game transition 5 200
        otherFrames <- renderToSVG' world gameId xs
        return $ frames ++ otherFrames
renderToSVG' world gameId ((transition@DrawGame{from=game}):xs) =
    do
        frames <- animateFrames world game transition 5 200
        otherFrames <- renderToSVG' world gameId xs
        return $ frames ++ otherFrames

renderToSVG' world gameId ((transition@PlaceToken{from=game}):xs) =
    do
        frames <- animateFrames world game transition 10 60
        otherFrames <- renderToSVG' world gameId xs
        return $ frames ++ otherFrames
renderToSVG' world gameId ((transition@StartTurn{from=game}):xs) =
    do
        frames <- animateFrames world game transition 5 200
        otherFrames <- renderToSVG' world gameId xs
        return $ frames ++ otherFrames
renderToSVG' world gameId (transition:xs) = renderToSVG' world gameId xs
renderToSVG' world gameId [] = return []

animateFrames :: World -> Game -> Transition -> Integer -> Integer -> IO [Frame String]
animateFrames world game transition numberOfFrames frameIntervalMillis =
    do
        let maxFrame = (numberOfFrames-1)
        let frameIndicies = [1..maxFrame]
        let renders = DL.map (\index -> renderGame world game (Just (transition, (fromInteger index) / (fromInteger maxFrame)))) frameIndicies

        DT.mapM (\render -> do
            svg <- render
            return Frame{durationMillis=frameIntervalMillis, value=SS.renderSvg svg} ) renders

renderGame :: World -> Game -> Maybe (Transition, Float) -> IO S.Svg
renderGame world game@Game{board=Board{width=tilesAcross, height=tilesDown}, participants=participants} maybeTransitionAndProgress =
    do
        renderedParticipants <- renderParticipants world participants 0 participantX participantY imageWidth imageHeight 0 tileHeight
        renderedTokens <- renderTokens world game tileWidth tileHeight imageWidth imageHeight
        renderedTransition <- renderTransition maybeTransitionAndProgress
        return $ S.docTypeSvg ! A.version "1.1" ! A.width (S.stringValue $ show width) ! A.height (S.stringValue $ show height) ! A.viewbox (S.stringValue $ "0 0 "++(show width)++" "++(show height)) $ do
                S.g $ do
                    S.rect ! A.width (S.stringValue $ show width) ! A.height (S.stringValue $ show height) ! A.fill "white" ! A.stroke "none" ! A.strokeWidth "2"
                    renderedParticipants
                    S.g ! (A.transform $ S.translate boardX boardY) $ do
                        S.rect ! A.width (S.stringValue $ show boardWidth) ! A.height (S.stringValue $ show boardHeight) ! A.fill "none" ! A.stroke "black"
                        verticalLines (tilesAcross - 1) tileWidth 0 tileWidth boardHeight
                        horizontalLines (tilesDown - 1) 0 tileHeight boardWidth tileHeight
                        verticalChars tilesDown (boardWidth + tileWidth `quot` 2) (boardHeight - tileHeight `quot` 8) tileHeight tileHeight
                        horizontalChars (tilesAcross - 1) (boardWidth - tileWidth `quot` 2) (boardHeight + tileHeight - tileHeight `quot` 8) tileWidth tileWidth
                        renderedTokens
                    renderedTransition
        where
            tileWidth = 40
            tileHeight = 40
            imageWidth = 30
            imageHeight = 30
            boardWidth = tileWidth * tilesAcross
            boardHeight = tileHeight * tilesDown
            width = 506
            height = 256
            boardMargin = (height - boardHeight) `quot` 2
            boardX = width - boardWidth - boardMargin
            boardY = boardMargin
            participantX = boardMargin
            participantY = boardMargin + (tileHeight - imageHeight) `quot` 2

            renderTransition :: Maybe (Transition, Float) -> IO S.Svg
            renderTransition Nothing = return $ S.title
            renderTransition (Just (StartTurn{from=game, newParticipantIndex=participantIndex}, progress)) =
                do
                    let displayName = participantDisplayName world game participantIndex
                    return $ S.text_ ! A.x (S.stringValue $ show (width `quot` 2) ) ! A.y (S.stringValue $ show $ boardMargin - boardMargin `quot` 4) ! A.fontSize (S.stringValue $ show $ boardMargin `quot` 2) ! A.textAnchor "middle" ! A.opacity (S.stringValue $ show progress) $ do
                        T.text $ DSC.cs $ displayName ++ "'s turn"
            renderTransition (Just (PlaceToken{row=row, column=column, tokenParticipantIndex=participantIndex}, progress)) =
                do
                    let sx = participantX
                    let sy = participantY + participantIndex * tileHeight
                    let dx = boardX + column * tileWidth + (tileWidth - imageWidth) `quot` 2 - sx
                    let dy = boardY + row * tileHeight + (tileHeight - imageHeight) `quot` 2 - sy
                    renderTokenType world game (ParticipantTokenType participantIndex) (floor ((fromInteger sx) + (fromInteger dx) * progress)) (floor ((fromInteger sy) + (fromInteger dy) * progress)) imageWidth imageHeight
            renderTransition (Just (WinGame{winnerIndex=winnerIndex}, progress)) =
                do
                    let displayName = participantDisplayName world game winnerIndex
                    return $ S.text_ ! A.x (S.stringValue $ show (width `quot` 2) ) ! A.y (S.stringValue $ show $ boardMargin - boardMargin `quot` 4) ! A.fontSize (S.stringValue $ show $ boardMargin `quot` 2) ! A.textAnchor "middle" ! A.opacity (S.stringValue $ show progress) ! A.fill "red" $ do
                        T.text $ DSC.cs $ displayName ++ " won!"
            renderTransition (Just (DrawGame{}, progress)) =
                do
                    return $ S.text_ ! A.x (S.stringValue $ show (width `quot` 2) ) ! A.y (S.stringValue $ show $ boardMargin - boardMargin `quot` 4) ! A.fontSize (S.stringValue $ show $ boardMargin `quot` 2) ! A.textAnchor "middle" ! A.opacity (S.stringValue $ show progress) ! A.fill "blue" $ do
                        T.text $ "Draw!"



renderTokens :: World -> Game -> Integer -> Integer -> Integer -> Integer -> IO S.Svg
renderTokens world game@Game{board=Board{tokens=tokens}} tileWidth tileHeight tokenWidth tokenHeight =
    do
        renders <- DT.mapM (\token -> renderToken world game token tileWidth tileHeight tokenWidth tokenHeight) tokens
        return $ DL.foldl (\svg existing -> svg >> existing) (S.title) renders

renderToken :: World -> Game -> Token -> Integer -> Integer -> Integer -> Integer -> IO S.Svg
renderToken world game token@Token{x=x, y=y, tokenType=tokenType} tileWidth tileHeight tokenWidth tokenHeight =
    do
        let tx = x * tileWidth + (tileWidth - tokenWidth) `quot` 2
        let ty = y * tileHeight + (tileHeight - tokenHeight) `quot` 2
        renderTokenType world game tokenType tx ty tokenWidth tokenHeight

renderTokenType :: World -> Game -> TokenType -> Integer -> Integer -> Integer -> Integer -> IO S.Svg
renderTokenType world game@Game{participants=participants} (ParticipantTokenType participantIndex) x y w h =
    do
        let participant = participants!!(fromInteger participantIndex)
        renderParticipant world participant participantIndex x y w h False

renderParticipants :: World -> [Participant] -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO S.Svg
renderParticipants world (participant:otherParticipants) playerNumber x y w h dx dy =
    do

        image <- renderParticipant world participant playerNumber x y w h True
        if DL.null otherParticipants
            then return image
            else do
                otherImages <- renderParticipants world otherParticipants (playerNumber+1) (x+dx) (y+dy) w h dx dy
                return $ image >> otherImages


renderParticipant :: World -> Participant -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool -> IO S.Svg
renderParticipant World{players=players} participant playerNumber x y w h showDisplayName =
    do
        let (imageURL, displayName) = do
            case participant of
                PlayerParticipant playerId ->
                    do
                        let (Just Player{playerDetails=PlayerDetails{displayName=playerDisplayName, imageURL=playerImageURL}}) = DM.lookup playerId players
                        (playerImageURL, "@"++playerDisplayName)
                _ -> (Nothing, "AI")
        dataURI <- do
            case imageURL of
                Just url ->
                    do
                        -- cairo can only handle local PNG images!
                        toPNGDataURI url
                _ -> return Nothing
        let image = case dataURI of
                Just uri -> S.image ! A.x (S.stringValue $ show x) ! A.y (S.stringValue $ show y) ! A.width (S.stringValue $ show w) ! A.height (S.stringValue $ show h) ! A.xlinkHref (S.stringValue uri)
                _ -> S.text_ ! A.x (S.stringValue $ show (x + w `quot` 2) ) ! A.y (S.stringValue $ show (y + h)) ! A.fontSize (S.stringValue $ show h) ! A.textAnchor "middle" $ do
                    T.text $ DSC.cs $ "P" ++ (show $ playerNumber + 1)
        let text = S.text_ ! A.x (S.stringValue $ show (x + w + h `quot` 2) ) ! A.y (S.stringValue $ show (y + ((3 * h) `quot` 4))) ! A.fontSize (S.stringValue $ show (h `quot` 2)) ! A.textAnchor "left" $ do
            T.text $ DSC.cs $ displayName
        return $ if showDisplayName
            then image >> text
            else image


getProfileImageData :: String -> IO (Either String DBS.ByteString)
getProfileImageData url =
    do
        let filePath = map (\c -> case c of
                '/' -> '_'
                ':' -> '_'
                '?' -> '_'
                '+' -> '_'
                '%' -> '_'
                _ -> c) url
        let dir = "temp"
        let fullFilePath = dir ++ "/" ++ filePath;

        exists <- Dir.doesFileExist fullFilePath
        if exists
            then
                do
                    bs <- DBS.readFile fullFilePath
                    return $ Right bs
            else
                do
                    bs <- NCD.openURIWithOpts [NCO.CurlTimeout 30, NCO.CurlConnectTimeout 10] url
                    case bs of
                        Right rawData ->
                            do
                                Dir.createDirectoryIfMissing True dir
                                DBS.writeFile fullFilePath rawData
                        _ -> return ()
                    return bs

toPNGDataURI :: String -> IO (Maybe String)
toPNGDataURI url =
    do
        -- check local cache!
        urlResult <- getProfileImageData url
        case urlResult of
            Left s ->
                do
                    putStrLn $ "unable to download from "++url++": "++s
                    return Nothing
            Right rawData ->
                do
                    let decodedImage = CP.decodeImage rawData
                    case decodedImage of
                        Left s ->
                            do
                                putStrLn $ "unable to decode image "++url++": "++s
                                return Nothing
                        Right decodedImage ->
                            do
                                let encodedPNG = CP.encodeDynamicPng decodedImage
                                case encodedPNG of
                                    Left s ->
                                        do
                                            putStrLn $ "unable to encode image "++url++": "++s
                                            return Nothing
                                    Right encodedData ->
                                        do
                                            let strictEncodedData = DBS.concat . DBSLZ.toChunks $ encodedData
                                            return $ Just $ "data:image/png;base64,"++(DBSC.unpack $ DBS64.encode $ strictEncodedData)


horizontalChars :: Integer -> Integer -> Integer -> Integer -> Integer -> S.Svg
horizontalChars count x y dx h =
    do
        let char = [DC.chr $ (DC.ord 'A') + (fromInteger count)]
        let text = S.text_ ! A.x (S.stringValue $ show x) ! A.y (S.stringValue $ show y) ! A.fontSize (S.stringValue $ show h) ! A.textAnchor "middle" $ do
                    T.text $ DSC.cs $ char
        if count > 0
            then text >> (horizontalChars (count - 1) (x - dx) y dx h)
            else text

verticalChars :: Integer -> Integer -> Integer -> Integer -> Integer -> S.Svg
verticalChars count x y dy h =
    do
        let char = show count
        let text = S.text_ ! A.x (S.stringValue $ show x) ! A.y (S.stringValue $ show y) ! A.fontSize (S.stringValue $ show h) ! A.textAnchor "middle" $ do
                    T.text $ DSC.cs $ char
        if count > 1
            then text >> (verticalChars (count - 1) x (y - dy) dy h)
            else text

verticalLines :: Integer -> Integer -> Integer -> Integer -> Integer -> S.Svg
verticalLines count x y dx height =
    do
        let line = S.path ! A.stroke "black" ! A.fill "none" ! (A.d $ S.mkPath $ do
            S.m x y
            S.l x (y + height))
        if count > 0
            then line >> (verticalLines (count-1) (x+dx) y dx height)
            else line

horizontalLines :: Integer -> Integer -> Integer -> Integer -> Integer -> S.Svg
horizontalLines count x y width dy =
    do
        let line = S.path ! A.stroke "black" ! A.fill "none" ! (A.d $ S.mkPath $ do
            S.m x y
            S.l (x + width) y)
        if count > 0
            then line >> (horizontalLines (count - 1) x (y + dy) width dy)
            else line
