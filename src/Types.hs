{-# LANGUAGE OverloadedStrings #-}

--
-- Definitions for HTTP types
--

module Types where

import Data.ByteString (ByteString)

data HttpMethod = Get | Put | Post | Delete
    deriving (Show, Eq)

newtype HttpPath = HttpPath ByteString
    deriving (Show, Eq)

newtype HttpVersion = HttpVersion (ByteString, ByteString)
    deriving (Show, Eq)

newtype HttpHeader = HttpHeader (ByteString, ByteString)
    deriving (Show, Eq)

type HttpHeaders = [HttpHeader]

newtype HttpBody = HttpBody ByteString
    deriving (Show, Eq)

data HttpRequestHead = HttpRequestHead HttpMethod HttpPath HttpVersion HttpHeaders
    deriving (Show, Eq)

data HttpRequest = HttpRequest HttpRequestHead HttpBody
    deriving (Show, Eq)
