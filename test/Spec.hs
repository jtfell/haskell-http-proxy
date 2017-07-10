{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit hiding (path)
import Data.Attoparsec.ByteString
import Data.ByteString
import Data.Either

import Parser

-- Taken from https://hackage.haskell.org/package/either-unwrap-1.1
fromRight           :: Either a b -> b
fromRight (Left _)  = error "fromRight: Argument takes form 'Left _'" 
fromRight (Right x) = x

-- Helper for asserting that a parsed input will result in a concrete output, 
assertParse :: (Eq a, Show a) => Parser a -> String -> ByteString -> a -> Assertion
assertParse p l i r = assertEqual l r $ fromRight $ parseOnly p i

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

requestTests = TestList [
     TestCase $ assertParse request "Minimal Request" "GET / HTTP/1.1\nBODY"
                  (HttpRequest Get (HttpPath "/") (HttpVersion ("1", "1")) (HttpBody "BODY"))
  ]

main = runTestTT $ TestList [
    TestLabel "HttpMethod" methodTests
  , TestLabel "HttpPath" pathTests
  , TestLabel "HttpVersion" versionTests
  , TestLabel "HttpRequest" requestTests
  ]
