module Main where

import Synth.Server qualified as Server
import Synth.Source qualified as Source
import Synth.GUI    qualified as GUI

import Control.Monad.Managed

import Data.Default (def)

main :: IO ()
main = runManaged do
    liftIO $ putStrLn startupMessage
    managed_ Server.withRunning
    source <- managed $ Source.withStarted def
    liftIO $ GUI.start def source

startupMessage :: String
startupMessage = "\
    \FigSynth <https://github.com/Swarthe/figsynth>\n\
    \This program comes with ABSOLUTELY NO WARRANTY.\n\
    \This is free software, and you are welcome to redistribute\n\
    \it under the conditions of the GPLv3 license.\n\
    \"
