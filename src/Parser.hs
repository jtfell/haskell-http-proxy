{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Super basic HTTP parser using the attoparsec library.
--


module Parser where

import Prelude hiding (takeWhile)
import Types
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser, parse, parseOnly,
                                   takeWhile, notInClass, takeByteString, sepBy)
import Data.Attoparsec.ByteString.Char8 (space, isDigit_w8, stringCI)

--
-- Helpers functions
--
eol = takeWhile (\w -> w == 13 || w == 10)
noEol = takeWhile (\w -> not (w == 13 || w == 10))
digit = takeWhile isDigit_w8

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
path = HttpPath <$> takeWhile (notInClass " ")

-- Ignore the HTTP/ and take a digit, a dot and another digit
version :: Parser HttpVersion
version = HttpVersion <$> liftA2 (,) (stringCI "HTTP/" *> digit) (stringCI "." *> digit)

header :: Parser HttpHeader
header = HttpHeader <$> liftA2 (,) takeLabel (stringCI ": " *> noEol)
    where takeLabel = takeWhile (notInClass ": \n\r")

headers :: Parser HttpHeaders
headers = header `sepBy` eol

-- Keep the rest of the request for passing on
body :: Parser HttpBody
body = HttpBody <$> takeByteString

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
parseRequest = parseOnly request
