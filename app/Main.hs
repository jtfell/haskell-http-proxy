{-# LANGUAGE OverloadedStrings #-}

--
-- Basic HTTP Reverse Proxy Server. Purposely constrained by not using any HTTP-specific dependencies.
--
-- Current limitations:
--   * Hardcoded backend server port (and no other configuration either really)
--   * Unknown performance
--

module Main where

import Network (listenOn, accept, PortID(..), Socket)
import Network.Socket hiding (recv, accept)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import System.IO (hPrint, hGetLine, hPutStrLn, Handle, hSetBuffering,
                  BufferMode(NoBuffering), hGetChar)
import Control.Concurrent (forkIO)
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Attoparsec.ByteString (maybeResult, parseWith)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import Types
import Parser
import PrettyPrinter

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
readRequest hdl = do
    msg <- getBytes hdl 10
    -- Incrementally get more input from the input handle until the request is done
    -- (Uses parseWith from attoparsec)
    parsedReq <- parseWith (getBytes hdl 10) request msg
    return $ fromMaybe nullReq $ maybeResult parsedReq

-- TODO: This is to skip the typechecker for now - Should send an error message
-- back if parsing fails.
nullReq :: HttpRequest
nullReq = undefined

-- Recursively create a Bytestring from the handler (blocking)
getBytes :: Handle -> Int -> IO BS.ByteString
getBytes hdl 0 = return ""
getBytes hdl num = do
    restOfBytes <- getBytes hdl (num - 1)
    thisByte <- hGetChar hdl
    return $ BS.cons (BS.c2w thisByte) restOfBytes

sendRequest :: Handle -> HttpRequest -> IO ()
sendRequest hdl req = hPrint hdl $ printRequest req

proxyRequest :: HttpRequest -> IO HttpRequest
proxyRequest req = do

    -- Interrogate DNS for localhost:3000 (Hardcoded for development simplicity)
    addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
    let serveraddr = head addrinfos

    -- Create the TCP socket and connect to it
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
    return req
    -- return $ fromRight $ parseRequest msg


fromRight           :: (Show a) => Either a b -> b
fromRight (Left x)  = error ("fromRight: Argument takes form 'Left " ++ show x ++ "'")
fromRight (Right x) = x
