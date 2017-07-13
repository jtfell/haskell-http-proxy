{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network (listenOn, accept, PortID(..), Socket)
import Network.Socket hiding (recv, accept)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import System.IO (hPrint, hGetLine, hPutStrLn, Handle, hSetBuffering,
                  BufferMode(NoBuffering))
import Control.Concurrent (forkIO)
import Data.Monoid

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

-- Handle requests from the socket concurrently - recursively spawns
-- lightweight green threads.
sockHandler :: Socket -> IO ()
sockHandler sock = do
    (hdl, _, _) <- accept sock
    forkIO $ handler hdl
    sockHandler sock

-- Reads a request, sends it to the "ProxyPass" location and relays the
-- response back to the original client.
handler :: Handle -> IO ()
handler hdl = do
     hSetBuffering hdl NoBuffering
     req <- readRequest hdl 
     res <- proxyRequest req
     sendRequest hdl res

--
-- External interactions for each request
--
readRequest :: Handle -> IO HttpRequest
readRequest hdl = (fromRight . parseRequest) <$> BS.hGetContents hdl

sendRequest :: Handle -> HttpRequest -> IO ()
sendRequest hdl req = hPrint hdl $ printRequest req

proxyRequest :: HttpRequest -> IO HttpRequest
proxyRequest req = do

    -- Interrogate DNS for localhost:3000
    addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
    let serveraddr = head addrinfos

    -- Create the TCP socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)

    -- Send the request over the TCP socket
    print $ "Sending to backend:   " <> printRequest req 
    sendAll sock $ printRequest req

    -- Get the response back from the backend server
    msg <- recv sock 1024
    print $ "Received from backend:   " <> msg

    -- Close the socket
    close sock

    -- Return the parsed response for sending back to the original client
    return $ fromRight $ parseRequest msg

fromRight           :: (Show a) => Either a b -> b
fromRight (Left x)  = error ("fromRight: Argument takes form 'Left " ++ show x ++ "'")
fromRight (Right x) = x

