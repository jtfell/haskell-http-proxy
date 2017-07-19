{-# LANGUAGE OverloadedStrings #-}

module ResponseSpec (
  parseResponseTests,
  printReponseTests
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

parseResponseTests = TestList [
     TestCase $ assertParse response "Basic Response"
                  "HTTP/1.1 200 OK\r\nHost: localhost:4000\r\nUser-Agent: curl/7.51.0\r\nAccept: */*\r\n\r\n"
                  (HttpResponse 
                    (HttpResponseHead (HttpVersion ("1", "1")) (HttpStatus 200 "OK") [
                      HttpHeader ("Host", "localhost:4000")
                    , HttpHeader ("User-Agent", "curl/7.51.0")
                    , HttpHeader ("Accept", "*/*")
                    ])
                    (HttpBody "")
                  )
  ]

printReponseTests = TestList [
      TestCase $ assertEqual "Print simple response"
          "HTTP/1.1 404 Not found\r\n\r\n"
          $ printResponse (HttpResponse (HttpResponseHead (HttpVersion ("1", "1")) (HttpStatus 303 "Not found") []) (HttpBody ""))
  ]

