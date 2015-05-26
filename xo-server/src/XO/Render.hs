module XO.Render where

import Data.Maybe (Maybe)

data Render a = Render {
        message :: Maybe String,
        frames :: [Frame a]
    } deriving Show

-- TODO add in image data
data Frame a = Frame {
        durationMillis :: Integer,
        value :: a
    } deriving Show

