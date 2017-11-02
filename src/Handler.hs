{-# LANGUAGE OverloadedStrings #-}

module Handler where

import System.IO (Handle, hSetBuffering,BufferMode(NoBuffering),
              hIsEOF, IOMode(ReadWriteMode))
import Network.Socket hiding (recv, accept)
import Network.Socket.ByteString (recv, sendAll)

import Data.Monoid
import Data.Maybe
import Data.Attoparsec.ByteString (maybeResult, parseWith)
import qualified Data.ByteString as BS

import Control.Monad (unless)
import Control.Concurrent (forkIO)

import Pipes
import Pipes.Parse (Parser, evalStateT, runStateT)
import Pipes.Attoparsec (parse)

import Types
import Parser
import PrettyPrinter

-- Reads a request, sends it to the "ProxyPass" location and relays the
-- response back to the original client.
handler :: Handle -> IO ()
handler hdl = do

    -- Create a new handle for the backend server
    backendHdl <- createNewRequest
    hSetBuffering hdl NoBuffering
    hSetBuffering backendHdl NoBuffering

    -- Set up a pipe for client -> backend
    forkIO $ runEffect $ fromNetworkReq hdl >-> serialiserP >-> toNetwork backendHdl

    return ()

    -- Set up a pipe for backend -> client
    -- runEffect $ fromNetwork backendHdl >-> pipesResponseParser >-> serialiserP >-> toNetwork hdl

-- Wraps a producer in a parser to incrementally parse the request from the network, then
-- produce a HttpRequest Type when its complete
fromNetworkReq :: Handle -> Producer HttpRequest IO ()
fromNetworkReq hdl = do
    (res, prod) <- runStateT (parse request) (fromNetwork hdl)
    case res of
        Nothing          -> lift $ "no Error"
        Just (Right req) -> yield req
        Just (Left err)  -> lift $ "Error"

fromNetwork :: Handle -> Producer BS.ByteString IO ()
fromNetwork hdl = do
    eof <- lift $ hIsEOF hdl
    unless eof $ do
        bs <- lift $ BS.hGetNonBlocking hdl 1024
        yield bs
        fromNetwork hdl

toNetwork :: Handle -> Consumer BS.ByteString IO ()
toNetwork hdl = do
    bytes <- await
    lift $ BS.hPut hdl bytes
    toNetwork hdl

-- pipesRequestParser :: Parser BS.ByteString HttpRequest
pipesRequestParser = (parse request)
pipesResponseParser = (parse response)

-- parserP = runStateT pipesRequestParser
--   bytes <- await
--   let result = pipesRequestParser bytes
--   yield result

serialiserP :: Pipe HttpRequest BS.ByteString IO ()
serialiserP = do
    req <- await
    yield $ printRequest req
    serialiserP

createNewRequest :: IO Handle
createNewRequest = do
    -- Interrogate DNS for localhost:3000 (Hardcoded for development simplicity)
    addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
    let serveraddr = head addrinfos

    -- Create the TCP socket and connect to it
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)

    -- Convert it to a handle and return it
    socketToHandle sock ReadWriteMode

