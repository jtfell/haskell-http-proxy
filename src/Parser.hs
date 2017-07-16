{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Super basic HTTP parser using the attoparsec library.
--
-- Based on the spec as outlined here:
-- https://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html
--

module Parser where

import Prelude hiding (takeWhile, take)
import Types
import Control.Applicative
import Data.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Attoparsec.ByteString (Parser, parse, parseOnly, takeWhile, take,
                                   notInClass, takeByteString, sepBy, satisfy,
                                   Result, parseWith)
import Data.Attoparsec.ByteString.Char8 (space, isDigit_w8, stringCI)

-- As we don't care about the string after we have parsed it to one
-- of the HTTP verbs, we can throw away the result of the "stringCI"
-- combinators, keeping only the HttpMethod ADT
method :: Parser HttpMethod
method = get <|> put <|> post <|> delete
    where get = stringCI "GET" *> pure Get
          put = stringCI "PUT" *> pure Put
          post = stringCI "POST" *> pure Post
          delete = stringCI "DELETE" *> pure Delete

-- Take all valid characters (takeWhile) and lift that into the applicative Parser context
path :: Parser HttpPath
path = HttpPath <$> takeWhile (notInClass " ")

-- Ignore the HTTP/ and take a digit, a dot and another digit
version :: Parser HttpVersion
version = HttpVersion <$> liftA2 (,) (stringCI "HTTP/" *> digit) (stringCI "." *> digit)

-- Header names and values are separated by ": " and placed in a tuple.
header :: Parser HttpHeader
header = HttpHeader <$> liftA2 (,) takeLabel (stringCI ": " *> noEol)
    where takeLabel = takeWhile (notInClass ": \n\r")

-- The list of headers are separated only by newlines
headers :: Parser HttpHeaders
headers = header `sepBy` eol

-- Keep the rest of the request for passing on
body :: Int -> Parser HttpBody
body length = HttpBody <$> take length

-- Combine all the bits of the request into a singular parser, separated by spaces/newlines
--
-- Note that there is an extra EOL between the last header and the body
requestHead :: Parser HttpRequestHead
requestHead = HttpRequestHead
    <$> method
    <*> (space *> path)
    <*> (space *> version)
    <*> (eol *> headers <* eol)

--
-- Parse the head, then use it to determine how long the body is and take that many bytes
--
request :: Parser HttpRequest
request = do
    reqHead <- requestHead
    eol
    let bodyLength = getBodyLength reqHead
    reqBody <- body bodyLength
    return (HttpRequest reqHead reqBody)

--
-- Helper functions
--
eol = stringCI "\r\n"
noEol = takeWhile (\w -> not (w == 13 || w == 10))
digit = takeWhile isDigit_w8

--
-- Inspect the headers/HTTP verb to determine the length of the body
--
getBodyLength :: HttpRequestHead -> Int
getBodyLength (HttpRequestHead m p v h)
    | m == Get = 0
    | hasContentLength h = getContentLength h
    -- TODO: implement chunking encoding stuff
    | otherwise = 0

--
-- Check if there is a content length header
--
hasContentLength :: HttpHeaders -> Bool
hasContentLength [] = False
hasContentLength (HttpHeader (x,y):xys)
    | x == "Content-Length" = True
    | otherwise = hasContentLength xys

--
-- Get the content length header value (or 0 if it doesn't exist)
--
getContentLength :: HttpHeaders -> Int
getContentLength [] = 0
getContentLength (HttpHeader (x,y):xys)
    | x == "Content-Length" = fst (fromMaybe (0, "") (readInt y))
    | otherwise = getContentLength xys

