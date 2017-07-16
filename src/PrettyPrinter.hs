{-# LANGUAGE OverloadedStrings #-}

--
-- Pretty printer for HttpRequests
-- Makes use of the mappend operator (<>) to combine ByteStrings as they are monoids
--

module PrettyPrinter where

import Types
import Data.Monoid
import Data.ByteString (ByteString)

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
printHeaders x = mconcat $ map (\h -> printHeader h <> "\r\n") x

printHeader :: HttpHeader -> ByteString
printHeader (HttpHeader (l, v)) = l <> ": " <> v

printBody :: HttpBody -> ByteString
printBody (HttpBody b) = b

printRequestHead :: HttpRequestHead -> ByteString
printRequestHead (HttpRequestHead m p v h) =
    printMethod m <> " " <>
    printPath p <> " " <> 
    printVersion v <> "\r\n" <> 
    printHeaders h

printRequest :: HttpRequest -> ByteString
printRequest (HttpRequest h b) =
    printRequestHead h <> "\r\n" <> printBody b

