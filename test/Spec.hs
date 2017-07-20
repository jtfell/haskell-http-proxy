{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit hiding (path)
import Data.Attoparsec.ByteString
import Data.ByteString

import Parser
import Types

import ResponseSpec
import RequestSpec

-- Modified from https://hackage.haskell.org/package/either-unwrap-1.1
fromRight           :: (Show a) => Either a b -> b
fromRight (Left x)  = error ("Failed with: 'Left " ++ show x ++ "'")
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

statusTests = TestList [
     TestCase $ assertParse status "Basic status" "200 OK" (HttpStatus 200 "OK")
     -- Is this the best behaviour here? Validation should happen later
  ,  TestCase $ assertParse status "Non-existent status code" "390 WEW" (HttpStatus 390 "")
  ]

body5 = body (Length 5)
bodyChunked = body Chunked

bodyTests = TestList [
     TestCase $ assertParse body5 "With content length" "hello" (HttpBody "hello")
     -- Example from wikipedia page
  ,  TestCase $ assertParse bodyChunked "With chunked encoding" "4\r\nWiki\r\n5\r\npedia\r\nE\r\n in\r\n\r\nchunks.\r\n0\r\n\r\n" (HttpBody "Wikipedia in\r\n\r\nchunks.")
  ]

main = runTestTT $ TestList [
    TestLabel "HttpMethod" methodTests
  , TestLabel "HttpPath" pathTests
  , TestLabel "HttpVersion" versionTests
  , TestLabel "HttpHeader" headerTests
  , TestLabel "HttpStatus" statusTests
  , TestLabel "HttpBody" bodyTests
  , TestLabel "Parse HttpRequest" parseRequestTests
  , TestLabel "Print HttpResponse" printRequestTests
  , TestLabel "Parse HttpRequest" parseResponseTests
  , TestLabel "Print HttpResponse" printRequestTests
  ]
