{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Types

import Network (listenOn, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hGetLine, hPutStrLn, Handle)
import Control.Concurrent (forkIO)

import Lib (constructNewRequest)

-- Start up the server listening on the specified port
main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    print $ "Listening on " ++ (head args)
    sockHandler sock

-- Handle requests from the socket concurrently - uses lightweight threads
sockHandler :: Socket -> IO ()
sockHandler sock = do
    (hdl, _, _) <- accept sock
    forkIO $ proxyRequest hdl
    sockHandler sock

-- Creates a request and sends it to the "ProxyPass" location
proxyRequest :: Handle -> IO ()
proxyRequest hdl = do
    manager <- newManager defaultManagerSettings

    -- TODO: Is this the best way to read the incoming request
    vRequest <- hGetLine hdl

    -- Translate the incoming request to one directed at the backend server
    -- TODO: Get the URL from config
    request <- parseRequest $ constructNewRequest vRequest "http://localhost:1337"

    -- Do the request
    response <- httpLbs request manager

    -- Proxy the response back to the client
    hPutStrLn hdl $ show $ responseBody response

