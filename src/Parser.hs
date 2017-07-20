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
                                   Result, parseWith, inClass)
import Data.Attoparsec.ByteString.Char8 (space, isDigit_w8, stringCI)

-- As we don't care about the string after we have parsed it to one
-- of the HTTP verbs, we can throw away the result of the "stringCI"
-- combinators, keeping only the HttpMethod ADT
method :: Parser HttpMethod
method = get <|> put <|> post <|> delete <|> head <|> trace <|> connect <|> options <|> patch
    where get = stringCI "GET" *> pure Get
          put = stringCI "PUT" *> pure Put
          post = stringCI "POST" *> pure Post
          delete = stringCI "DELETE" *> pure Delete
          head = stringCI "HEAD" *> pure Head
          trace = stringCI "TRACE" *> pure Trace
          connect = stringCI "CONNECT" *> pure Connect
          options = stringCI "OPTIONS" *> pure Options
          patch = stringCI "PATCH" *> pure Patch

-- Take all valid characters
path :: Parser HttpPath
path = HttpPath <$> takeWhile (notInClass " \n\r")

-- Take all digits
status :: Parser HttpStatus
status = constructStatus . toInt <$> digit
    where toInt y = fst $ fromMaybe (0, "") (readInt y)

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

data BodyLength = Length Int | Chunked

-- Keep the rest of the request for passing on
body :: BodyLength -> Parser HttpBody
body (Length x) = HttpBody <$> take x
body Chunked = HttpBody <$> fmap mconcat chunks <* eol

chunk :: Parser ByteString
chunk = hexDigit >>= (\len -> eol *> take (hexDigitToInt len))

-- chunk = do
--     len <- hexDigit
--     eol
--     take $ hexDigitToInt len

chunks :: Parser [ByteString]
chunks = chunk `sepBy` eol

hexDigitToInt :: ByteString -> Int
hexDigitToInt "A" = 10
hexDigitToInt "B" = 11
hexDigitToInt "C" = 12
hexDigitToInt "D" = 13
hexDigitToInt "E" = 14
hexDigitToInt "F" = 15
hexDigitToInt x = fst (fromMaybe (0, "") (readInt x))

--
-- Combine the smaller parsers into one for the request and one for the response.
--
-- Both use the same pattern: parse the head, then use it to determine how long
-- the body is and take that many bytes
--
requestHead :: Parser HttpRequestHead
requestHead = HttpRequestHead
    <$> method
    <*> (space *> path)
    <*> (space *> version)
    <*> (eol *> headers <* eol)

request :: Parser HttpRequest
request = do
    reqHead <- requestHead
    eol
    let bodyLength = getReqBodyLength reqHead
    reqBody <- body bodyLength
    return (HttpRequest reqHead reqBody)

responseHead :: Parser HttpResponseHead
responseHead = HttpResponseHead
    <$> version
    <*> (space *> status)
    <* (space *> noEol)
    <*> (eol *> headers <* eol)

response :: Parser HttpResponse
response = do
    resHead <- responseHead
    eol
    let bodyLength = getResBodyLength resHead
    resBody <- body bodyLength
    return (HttpResponse resHead resBody)

--
-- Helper functions
--
eol = stringCI "\r\n"
noEol = takeWhile (\w -> not (w == 13 || w == 10))
digit = takeWhile isDigit_w8
hexDigit = takeWhile isHexDigit_w8
isHexDigit_w8 w = w - 48 <= 9 || w - 65 <= 5

--
-- Inspect the headers/HTTP verb to determine the length of the body
--
getReqBodyLength :: HttpRequestHead -> BodyLength
getReqBodyLength (HttpRequestHead m p v h)
    | m == Get = Length 0
    | hasContentLength h = Length (getContentLength h)
    | isChunkedEncoding h = Chunked
    | otherwise = Length 0

--
-- Inspect the headers/HTTP verb to determine the length of the body
--
getResBodyLength :: HttpResponseHead -> BodyLength
getResBodyLength (HttpResponseHead v s h)
    | isRedirectStatus s = Length 0
    | hasContentLength h = Length (getContentLength h)
    | isChunkedEncoding h = Chunked
    | otherwise = Length 0

--
-- Check if there is a content length header
--
hasContentLength :: HttpHeaders -> Bool
hasContentLength [] = False
hasContentLength (HttpHeader (x,y):xys)
    | x == "Content-Length" = True
    | otherwise = hasContentLength xys

isChunkedEncoding :: HttpHeaders -> Bool
isChunkedEncoding [] = False
isChunkedEncoding (HttpHeader (x,y):xys)
    | x == "Transfer-Encoding" && y == "chunked" = True
    | otherwise = isChunkedEncoding xys

--
-- Get the content length header value (or 0 if it doesn't exist)
--
getContentLength :: HttpHeaders -> Int
getContentLength [] = 0
getContentLength (HttpHeader (x,y):xys)
    | x == "Content-Length" = fst (fromMaybe (0, "") (readInt y))
    | otherwise = getContentLength xys

