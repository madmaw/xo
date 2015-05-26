module XO.Render.AnimatedGIF where

import XO.Render
import XO.Render.SVGCairo
import XO.Core
import XO.Core.Types

import qualified Codec.Picture as P
import qualified Codec.Picture.Gif as PG
import qualified Codec.Picture.Types as PT
import qualified Codec.Picture.ColorQuant as PQ

import Data.Either (Either)
import Data.ByteString (ByteString, index)
import qualified Data.ByteString.Lazy as DBSLZ
import qualified Data.List as DL

renderToAnimatedGIF :: World -> GameId -> [Transition] -> IO (Either String DBSLZ.ByteString)
renderToAnimatedGIF world gameId transitions =
    do
        rawFrames <- renderToRaw world gameId transitions
        let gifFrames = map convertRawFrame rawFrames
        if DL.length gifFrames == 1
            then
                do
                    let (palette, _, image) = DL.head gifFrames
                    return $ PG.encodeGifImageWithPalette image palette
            else return $ PG.encodeGifImages PG.LoopingForever gifFrames


convertRawFrame :: Frame RawFrameData -> (PT.Palette, PG.GifDelay, PT.Image PT.Pixel8)
convertRawFrame (Frame delayMillis frameData@RawFrameData{frameWidth=frameWidth, frameHeight=frameHeight}) =
    do
        let image = P.generateImage (getPixel frameData) frameWidth frameHeight
        let (image8, palette8) = PQ.palettize PQ.defaultPaletteOptions image
        (palette8, (fromInteger delayMillis) `quot` 10, image8)



getPixel :: RawFrameData -> Int -> Int -> PT.PixelRGB8
getPixel RawFrameData{frameWidth=frameWidth, frameHeight=frameHeight, raw=raw} x y =
    do
        let i = (y * frameWidth + x) * 4 -- skip one byte?
        let r = index raw $ i + 2
        let g = index raw $ i + 1
        let b = index raw $ i + 0
        PT.PixelRGB8 r g b
