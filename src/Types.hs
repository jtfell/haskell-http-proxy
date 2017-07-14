{-# LANGUAGE OverloadedStrings #-}

--
--

module Types where

import Data.ByteString (ByteString)

--
-- Definitions for HTTP types
--
data HttpMethod = Get | Put | Post | Delete
    deriving (Show, Eq)
newtype HttpPath = HttpPath ByteString
    deriving (Show, Eq)
newtype HttpVersion = HttpVersion (ByteString, ByteString)
    deriving (Show, Eq)
newtype HttpBody = HttpBody ByteString
    deriving (Show, Eq)
newtype HttpHeader = HttpHeader (ByteString, ByteString)
    deriving (Show, Eq)
type HttpHeaders = [HttpHeader]
data HttpRequest = HttpRequest HttpMethod HttpPath HttpVersion HttpHeaders HttpBody
    deriving (Show, Eq)
