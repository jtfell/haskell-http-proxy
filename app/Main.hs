--
-- Basic HTTP Reverse Proxy Server. Purposely constrained by not using any HTTP-specific dependencies.
--
-- Current limitations:
--   * Hardcoded backend server port (and no other configuration either really)
--   * Unknown performance
--

module Main where

import System.Environment (getArgs)

import Network (listenOn, accept, PortID(..), Socket)
import Control.Concurrent (forkIO)

import Handler

-- Start up the server listening on the specified port
main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    print $ "Listening on " ++ head args
    sockHandler sock

-- Handle requests from the socket concurrently - recursively spawns
-- lightweight green threads.
sockHandler :: Socket -> IO ()
sockHandler sock = do
    (hdl, _, _) <- accept sock
    forkIO $ handler hdl
    sockHandler sock

