{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit hiding (path)
import Data.Attoparsec.ByteString
import Data.ByteString

import Parser

-- Helper for asserting that a parsed input will result in a concrete output
assertParse :: (Eq a) => Parser a -> String -> ByteString -> a -> Assertion
assertParse t l i r = assertEqual l (Just True) $ compareResults (parse t i) (Done "" r)

methodTests = TestList [
     TestCase $ assertParse method "GET Request" "GET" Get
  ,  TestCase $ assertParse method "PUT Request" "PUT" Put
  ,  TestCase $ assertParse method "POST Request" "POST" Post
  ,  TestCase $ assertParse method "DELETE Request" "DELETE" Delete
  ]

pathTests = TestList [
     TestCase $ assertParse path "Simple path" "/example/path" (HttpPath "/example/path")
  ]

versionTests = TestList [
     TestCase $ assertParse version "HTTP 1.1" "HTTP/1.1" (HttpVersion ("1", "1"))
  ]

main = runTestTT $ TestList [
    TestLabel "HttpMethod" methodTests
  , TestLabel "HttpPath" pathTests
  , TestLabel "HttpVersion" versionTests
  ]
