{-# LANGUAGE OverloadedStrings #-}

module Parser (parseRequest) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, parse, Result)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as AC

-- First we define the ADT for the HttpRequests we will be parsing
data HttpMethod = Get | Put | Post | Delete
    deriving (Show)
newtype HttpPath = HttpPath ByteString
data HttpRequest = HttpRequest HttpMethod HttpPath

-- As we don't care about the string after we have parsed it to one
-- of the HTTP verbs, we can throw away the result of the "string BS"
-- combinators, keeping only the HttpMethod ADT
method :: Parser HttpMethod
method = get <|>
         put <|>
         post <|>
         delete
    where get = A.string "GET" *> pure Get
          put = A.string "PUT" *> pure Put
          post = A.string "POST" *> pure Post
          delete = A.string "DELETE" *> pure Delete

-- Take all valid characters (takeWhile) and lift that into the applicative
-- Parser context
path :: Parser HttpPath
path = HttpPath <$> A.takeWhile (A.inClass "a-zA-z0-9/")

request :: Parser request
request = undefined

parseRequest :: ByteString -> Result ByteString
parseRequest = parse request
