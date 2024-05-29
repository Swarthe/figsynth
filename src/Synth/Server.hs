module Synth.Server (withRunning) where

import Control.Exception

import Vivid (connectToSCServer, defaultConnectConfig)

import Control.Concurrent (threadDelay)

import System.Environment (getEnvironment)
import System.Process (CreateProcess, proc, withCreateProcess)
import System.Process qualified as Process

import Data.Functor ((<&>))

-- | Runs an IO operation in a context where a connection to the SuperCollider
-- server (@scsynth@) is active.
--
-- If SC is already running, the provided operation is executed directly and the
-- server is left as is. Otherwise, the server is started and is stopped once
-- the operation is complete.
withRunning :: IO a -> IO a
withRunning op = try connect >>= \case
    Left (fromException -> Just BlockedIndefinitelyOnMVar) ->
        -- SC might not be running; launch it.
        newProcessConf >>= flip withCreateProcess
            \_ _ _ _ -> awaitStartup >> connect >> op
    Left e -> throwIO e
    Right _ -> op
  where
    connect = connectToSCServer defaultConnectConfig

    -- Wait an arbitrary duration for SC to start before connecting.
    awaitStartup = threadDelay 300_000

newProcessConf :: IO CreateProcess
newProcessConf = newEnv <&> conf
  where
    newEnv = getEnvironment
        -- Necessary for SC to function.
        <&> ([ ("SC_JACK_DEFAULT_INPUTS",  "system")
             , ("SC_JACK_DEFAULT_OUTPUTS", "system")
             ] ++)

    conf env =
        -- Hardcoded port and low verbosity
        (proc "scsynth" ["-u", "57110", "-V", "-1"])
            { Process.env = Just env
            -- Suppress the noisy SuperCollider log.
            , Process.std_out = Process.NoStream }
