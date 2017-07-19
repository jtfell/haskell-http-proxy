{-# LANGUAGE OverloadedStrings #-}

--
-- Pretty printer for HttpRequests
-- Makes use of the mappend operator (<>) to combine ByteStrings as they are monoids
--

module PrettyPrinter where

import Types
import Data.Monoid
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

printMethod :: HttpMethod -> ByteString
printMethod m 
    | m == Get = "GET"
    | m == Put = "PUT"
    | m == Post = "POST"
    | m == Delete = "DELETE"
    | m == Head = "HEAD"
    | m == Trace = "TRACE"
    | m == Connect = "CONNECT"
    | m == Options = "OPTIONS"
    | m == Patch = "PATCH"

printPath :: HttpPath -> ByteString
printPath (HttpPath p) = p

printStatus :: HttpStatus -> ByteString
printStatus (HttpStatus c m) = (pack . show) c <> " " <> m

printVersion :: HttpVersion -> ByteString
printVersion (HttpVersion (v1,v2)) = "HTTP/" <> v1 <> "." <> v2

printHeaders :: HttpHeaders -> ByteString
printHeaders x = mconcat $ map (\h -> printHeader h <> "\r\n") x

printHeader :: HttpHeader -> ByteString
printHeader (HttpHeader (l, v)) = l <> ": " <> v

-- TODO: Need to re-encode as chunked I think? - otherwise the transfer-encoding header needs to be stripped
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

printResponseHead :: HttpResponseHead -> ByteString
printResponseHead (HttpResponseHead v s h) =
    printVersion v <> " " <> 
    printStatus s <> "\r\n" <>
    printHeaders h

printResponse:: HttpResponse -> ByteString
printResponse (HttpResponse h b) =
    printResponseHead h <> "\r\n" <> printBody b

