{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network (listenOn, accept, PortID(..), Socket)
import Network.Socket hiding (recv, accept)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import System.IO (hPrint, hGetLine, hPutStrLn, Handle)
import Control.Concurrent (forkIO)

import qualified Data.ByteString as BS

import Parser

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

    -- Efficient way to get bytestrings from the network (I think?)
    str <- BS.hGetContents hdl

    print str

    -- This is a pure function, so we can't bind it in the monadic context
    let req = fromRight $ parseRequest str

    -- Parse the bytestring and print the HTTP request object
    print $ printRequest req

    res <- sendRequest req

    print $ show res

    -- Proxy the response back to the client
    -- hPrint hdl $ show $ responseBody response

fromRight           :: (Show a) => Either a b -> b
fromRight (Left x)  = error ("fromRight: Argument takes form 'Left " ++ show x ++ "'")
fromRight (Right x) = x

sendRequest :: HttpRequest -> IO BS.ByteString
sendRequest req = do

    -- Interrogate DNS for localhost:3000
    addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
    let serveraddr = head addrinfos

    -- create the TCP socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)

    sendAll sock $ printRequest req
    msg <- recv sock 1024
    close sock

    return msg
