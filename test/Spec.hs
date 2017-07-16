{-# LANGUAGE OverloadedStrings #-}

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

methodTests = TestList [
     TestCase $ assertParse method "GET Request" "GET" Get
  ,  TestCase $ assertParse method "PUT Request" "PUT" Put
  ,  TestCase $ assertParse method "POST Request" "POST" Post
  ,  TestCase $ assertParse method "DELETE Request" "DELETE" Delete
  ]

pathTests = TestList [
     TestCase $ assertParse path "Root Path" "/" (HttpPath "/")
  ,  TestCase $ assertParse path "Simple Path" "/example" (HttpPath "/example")
  ]

versionTests = TestList [
     TestCase $ assertParse version "HTTP 1.1" "HTTP/1.1" (HttpVersion ("1", "1"))
  ]

headerTests = TestList [
     TestCase $ assertParse header "Basic header" "Cache-Control: none" (HttpHeader ("Cache-Control", "none"))
  ,  TestCase $ assertParse headers "Basic header list" "Cache-Control: none\r\nAccept: text/html"
          [HttpHeader ("Cache-Control", "none"), HttpHeader ("Accept", "text/html")]
  ]

requestTests = TestList [
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
  ,  TestCase $ assertParse request "Basic Get Request"
                  "POST / HTTP/1.1\r\nHost: localhost:4000\r\nContent-Length: 13\r\nAccept: */*\r\n\r\n12345678901233"
                  (HttpRequest 
                    (HttpRequestHead Post (HttpPath "/") (HttpVersion ("1", "1")) [
                      HttpHeader ("Host", "localhost:4000")
                    , HttpHeader ("Content-Length", "13")
                    , HttpHeader ("Accept", "*/*")
                    ])
                    (HttpBody "1234567890123")
                  )
  ]

-- printTests = TestList [
--       TestCase $ assertEqual "Print simple request"
--           "GET / HTTP/1.1\r\n\r\n"
--           $ printRequest (HttpRequest Get (HttpPath "/") (HttpVersion ("1", "1")) [] (HttpBody ""))
--   ]

main = runTestTT $ TestList [
    TestLabel "HttpMethod" methodTests
  , TestLabel "HttpPath" pathTests
  , TestLabel "HttpVersion" versionTests
  , TestLabel "HttpHeader" headerTests
  , TestLabel "HttpRequest" requestTests
--  , TestLabel "printRequest" printTests
  ]
