{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Types

import Network (listenOn, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hPrint, hGetLine, hPutStrLn, Handle)
import Control.Concurrent (forkIO)

import qualified Data.ByteString as BS

import Data.Attoparsec.ByteString as P

import Parser
import Lib (constructNewRequest)

-- Start up the server listening on the specified port
main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    print $ "Listening on " ++ head args
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

    -- Efficient way to get bytestrings from the network (I think?)
    str <- BS.hGetContents hdl

    -- Parse the bytestring and print the HTTP request object
    print $ show $ doParse str

    -- Translate the incoming request to one directed at the backend server
    -- TODO: Get the URL from config
    -- request <- parseRequest $ constructNewRequest vRequest "http://localhost:1337"

    -- Do the request
    -- response <- httpLbs request manager

    -- Proxy the response back to the client
    -- hPrint hdl $ show $ responseBody response

-- Not sure if this is the correct usage of attoparsec...
doParse str = case parse request str of
  P.Fail {}         -> error "Uh oh"
  P.Done contents _ -> contents
