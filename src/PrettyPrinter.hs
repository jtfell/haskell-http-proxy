{-# LANGUAGE OverloadedStrings #-}

--
-- Pretty printer for HttpRequests
--

module PrettyPrinter where

import Types
import Data.Monoid
import Data.ByteString (ByteString)

--
-- Makes use of the mappend operator (<>) to combine ByteStrings as they are a monoid
--
printRequest :: HttpRequest -> ByteString
printRequest (HttpRequest m p v h b) =
    printMethod m <> " " <>
    printPath p <> " " <> 
    printVersion v <> "\n" <> 
    printHeaders h <>
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

