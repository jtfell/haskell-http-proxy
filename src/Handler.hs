{-# LANGUAGE OverloadedStrings #-}

module Handler where

import System.IO (Handle, hSetBuffering,BufferMode(NoBuffering),
              hIsEOF, IOMode(ReadWriteMode))
import Network.Socket hiding (recv, accept)
import Network.Socket.ByteString (recv, sendAll)

import Data.Monoid
import Data.Maybe
import Data.Attoparsec.ByteString (maybeResult, parseWith, parse)
import qualified Data.ByteString as BS

import Control.Monad (unless)
import Control.Concurrent (forkIO)

import Pipes

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
    forkIO $ runEffect $ fromNetwork hdl >-> parserP >-> serialiserP >-> toNetwork backendHdl

    -- Set up a pipe for backend -> client
    runEffect $ fromNetwork backendHdl >-> parserP >-> serialiserP >-> toNetwork hdl


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

parserP :: Pipe BS.ByteString HttpRequest IO ()
parserP = undefined
-- TODO: Look into pipes-parse and pipes-attoparsec
-- parserP = do
--   bytes <- await
--   let result = parse request bytes
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

