{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, parse, Result)
import Data.Attoparsec.ByteString.Char8 (space, stringCI)
import Data.ByteString (ByteString, append)
import qualified Data.Attoparsec.ByteString as A

-- First we define the ADT for the HttpRequests we will be parsing
data HttpMethod = Get | Put | Post | Delete
    deriving (Show)
newtype HttpPath = HttpPath ByteString
    deriving (Show)
newtype HttpVersion = HttpVersion ByteString
    deriving (Show)
newtype HttpBody = HttpBody ByteString
    deriving (Show)
data HttpRequest = HttpRequest HttpMethod HttpPath HttpVersion HttpBody
    deriving (Show)

-- As we don't care about the string after we have parsed it to one
-- of the HTTP verbs, we can throw away the result of the "string BS"
-- combinators, keeping only the HttpMethod ADT
method :: Parser HttpMethod
method = get <|> put <|> post <|> delete
    where get = stringCI "GET" *> pure Get
          put = stringCI "PUT" *> pure Put
          post = stringCI "POST" *> pure Post
          delete = stringCI "DELETE" *> pure Delete

-- Take all valid characters (takeWhile) and lift that into the applicative
-- Parser context
path :: Parser HttpPath
path = HttpPath <$> A.takeWhile (A.notInClass " ")

-- Ignore the HTTP/ and take a digit, a dot and another digit
version :: Parser HttpVersion
version = HttpVersion <$> liftA2 append takeDigit (stringCI "." *> takeDigit)
    where takeDigit = A.takeWhile (A.inClass "0-9")

-- Keep the rest of the request for passing on
body :: Parser HttpBody
body = HttpBody <$> A.takeByteString

-- Combine all the bits of the request into a singular parser, separated by
-- spaces (and a newline)
request :: Parser HttpRequest
request = HttpRequest
    <$> method
    <*> (space *> path)
    <*> (space *> version)
    <*> (A.takeWhile isEndOfLine *> body)

-- Helper function for checking for EOL
isEndOfLine = A.inClass "\r\n"

-- Expose a function that takes a string and returns a parsed HttpRequest result
parseRequest :: ByteString -> Result HttpRequest
parseRequest = parse request
