{-# LANGUAGE OverloadedStrings #-}

module RequestSpec (
  parseRequestTests,
  printRequestTests
) where

import Test.HUnit hiding (path)
import Data.Attoparsec.ByteString
import Data.ByteString

import Parser
import Types
import PrettyPrinter

-- Modified from https://hackage.haskell.org/package/either-unwrap-1.1
fromRight           :: (Show a) => Either a b -> b
fromRight (Left x)  = error ("fromRight: Argument takes form 'Left " ++ show x ++ "'")
fromRight (Right x) = x

-- Helper for asserting that a parsed input will result in a concrete output
assertParse :: (Eq a, Show a) => Parser a -> String -> ByteString -> a -> Assertion
assertParse p l i x = assertEqual l x $ fromRight $ parseOnly p i

parseRequestTests = TestList [
     TestCase $ assertParse request "Basic Get Request"
                  "GET / HTTP/1.1\r\nHost: localhost:4000\r\nUser-Agent: curl/7.51.0\r\nAccept: */*\r\n\r\n"
                  (HttpRequest 
                    (HttpRequestHead Get (HttpPath "/") (HttpVersion ("1", "1")) [
                      HttpHeader ("Host", "localhost:4000")
                    , HttpHeader ("User-Agent", "curl/7.51.0")
                    , HttpHeader ("Accept", "*/*")
                    ])
                    (HttpBody "")
                  )
  ,  TestCase $ assertParse request "Basic POST Request"
                  "POST / HTTP/1.1\r\nHost: localhost:4000\r\nContent-Length: 13\r\nAccept: */*\r\n\r\n12345678901233"
                  (HttpRequest 
                    (HttpRequestHead Post (HttpPath "/") (HttpVersion ("1", "1")) [
                      HttpHeader ("Host", "localhost:4000")
                    , HttpHeader ("Content-Length", "13")
                    , HttpHeader ("Accept", "*/*")
                    ])
                    (HttpBody "1234567890123")
                  )
  ,  TestCase $ assertParse request "Post Request with chunked encoding"
                  "POST / HTTP/1.1\r\nHost: localhost:4000\r\nTransfer-Encoding: chunked\r\nAccept: */*\r\n\r\n7\r\nMozilla\r\n9\r\nDeveloper\r\n7\r\nNetwork\r\n0\r\n\r\n"
                  (HttpRequest 
                    (HttpRequestHead Post (HttpPath "/") (HttpVersion ("1", "1")) [
                      HttpHeader ("Host", "localhost:4000")
                    , HttpHeader ("Transfer-Encoding", "chunked")
                    , HttpHeader ("Accept", "*/*")
                    ])
                    (HttpBody "MozillaDeveloperNetwork")
                  )
  ]

printRequestTests = TestList [
      TestCase $ assertEqual "Print simple request"
          "GET / HTTP/1.1\r\n\r\n"
          $ printRequest (HttpRequest (HttpRequestHead Get (HttpPath "/") (HttpVersion ("1", "1")) []) (HttpBody ""))
  ]

