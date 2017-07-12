{-# LANGUAGE OverloadedStrings #-}

--
-- Super basic HTTP parser using the attoparsec library.
--

module Parser where

import Control.Applicative
import Data.Monoid
import Data.Attoparsec.ByteString (Parser, parse, Result)
import Data.Attoparsec.ByteString.Char8 (space, isDigit_w8, stringCI)
import qualified Data.Attoparsec.ByteString as A

import Data.ByteString (ByteString, append)
import Data.ByteString.Builder

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
parseRequest = A.parseOnly request

--
-- Define a pretty-printer for HttpRequests
-- 
-- Makes use of the mappend operator (<>) to combine ByteStrings as they are a monoid
--
printRequest :: HttpRequest -> ByteString
printRequest (HttpRequest m p v h b) =
    printMethod m <> " " <>
    printPath p <> " " <> 
    printVersion v <> "\n" <> 
    printHeaders h <> "\n" <>
    printBody b

printMethod :: HttpMethod -> ByteString
printMethod m 
    | m == Get = "GET"
    | m == Put = "PUT"
    | m == Post = "POST"
    | m == Delete = "DELETE"

printPath :: HttpPath -> ByteString
printPath (HttpPath p) = p

printVersion :: HttpVersion -> ByteString
printVersion (HttpVersion (v1,v2)) = "HTTP/" <> v1 <> "." <> v2

printHeaders :: HttpHeaders -> ByteString
printHeaders x = mconcat $ map (\h -> printHeader h <> "\n") x

printHeader :: HttpHeader -> ByteString
printHeader (HttpHeader (l, v)) = l <> ": " <> v

printBody :: HttpBody -> ByteString
printBody (HttpBody b) = b

