{-# LANGUAGE OverloadedStrings #-}

--
-- Super basic HTTP parser using the attoparsec library.
--

module Parser where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, parse, Result)
import Data.Attoparsec.ByteString.Char8 (space, isDigit_w8, stringCI)
import Data.ByteString (ByteString, append)
import qualified Data.Attoparsec.ByteString as A

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

--
-- Helpers functions
--
eol = A.takeWhile (\w -> w == 13 || w == 10)
noEol = A.takeWhile (\w -> not (w == 13 || w == 10))
digit = A.takeWhile isDigit_w8

--
-- Parsers for all the above types
--

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
path = HttpPath <$> A.takeWhile (A.notInClass " ")

-- Ignore the HTTP/ and take a digit, a dot and another digit
version :: Parser HttpVersion
version = HttpVersion <$> liftA2 (,) (stringCI "HTTP/" *> digit) (stringCI "." *> digit)

header :: Parser HttpHeader
header = HttpHeader <$> liftA2 (,) takeLabel (stringCI ": " *> noEol)
    where takeLabel = A.takeWhile (A.notInClass ": \n\r")

headers :: Parser HttpHeaders
headers = header `A.sepBy` eol

-- Keep the rest of the request for passing on
body :: Parser HttpBody
body = HttpBody <$> A.takeByteString

-- Combine all the bits of the request into a singular parser, separated by
-- spaces/newlines
request :: Parser HttpRequest
request = HttpRequest
    <$> method
    <*> (space *> path)
    <*> (space *> version)
    <*> (eol *> headers)
    <*> (eol *> body)

-- 
-- Expose a function that takes a string and returns a parsed HttpRequest result
--
parseRequest :: ByteString -> Result HttpRequest
parseRequest = parse request
