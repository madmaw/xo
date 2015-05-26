module XO.Conduit.Stdin (
    commandStdinSource
) where

import XO.Core.Commands

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Trans.Resource as RT

import Debug.Trace (trace)
import Control.Monad.IO.Class (liftIO)
import System.IO (stdin)

convertStringSourceToCommandSource :: C.Conduit String (RT.ResourceT IO) Command
convertStringSourceToCommandSource =
    do
        C.awaitForever readInput
    where
        readInput = \line -> do
            case reads line of
                [(command, "")] -> C.yield command
                _ -> C.yield InvalidCommand {
                        source = Nothing,
                        game = Nothing,
                        input = line,
                        reason = "parse error"
                    }


commandStdinSource :: C.Source (RT.ResourceT IO) Command  -- produces a list of commands
commandStdinSource = stdinLineSource C.$= convertStringSourceToCommandSource

stdinLineSource :: C.Source (RT.ResourceT IO) String
stdinLineSource = do
        line <- liftIO $ getLine
        C.yield line
        stdinLineSource


