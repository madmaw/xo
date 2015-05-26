module XO.Render.SVGCairo where

import XO.Render
import XO.Render.SVG
import XO.Core
import XO.Core.Types

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as CS
import Data.ByteString (ByteString)

data RawFrameData =
    RawFrameData {
        frameWidth :: Int,
        frameHeight :: Int,
        raw :: ByteString
    }

renderToRaw :: World -> GameId -> [Transition] -> IO [Frame RawFrameData]
renderToRaw world gameId transitions =
    do
        svgFrames <- renderToSVG world gameId transitions
        mapM transformSVGFrameToRawFrame svgFrames

transformSVGFrameToRawFrame :: Frame String -> IO (Frame RawFrameData)
transformSVGFrameToRawFrame Frame{durationMillis=durationMillis, value=value} =
    do
        putStrLn value
        svg <- CS.svgNewFromString value
        let (width, height) = CS.svgGetSize svg
        d <- C.withImageSurface C.FormatRGB24 width height $ \result -> do
            C.renderWith result $ do
                C.save
                C.setOperator C.OperatorClear
                C.paint
                C.restore
                CS.svgRender svg
            C.imageSurfaceGetData result
        let frameData = RawFrameData {
                frameWidth = width,
                frameHeight = height,
                raw = d
            }
        return Frame{durationMillis=durationMillis, value=frameData}
