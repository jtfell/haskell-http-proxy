{-# LANGUAGE OverloadedStrings #-}

--
-- Definitions for HTTP types
--

module Types where

import Data.ByteString (ByteString)

--
-- Generic types
--
data HttpMethod = Get | Put | Post | Delete
    deriving (Show, Eq)

data HttpStatus = HttpStatus Int ByteString
    deriving (Show)

-- All properties of the status type are based on the code not the message
instance Eq HttpStatus where
  HttpStatus a x == HttpStatus b y = a == b
instance Ord HttpStatus where
  HttpStatus a x `compare` HttpStatus b y = a `compare` b
instance Enum HttpStatus where
  fromEnum = getStatusCode
  toEnum 100 = HttpStatus 100 "Continue"
  toEnum 101 = HttpStatus 101 "Switching Protocols"
  toEnum 102 = HttpStatus 102 "Processing"

  toEnum 200 = HttpStatus 200 "OK"
  toEnum 201 = HttpStatus 201 "Created"
  toEnum 202 = HttpStatus 202 "Accepted"
  toEnum 203 = HttpStatus 203 "Non-authoritative Information"
  toEnum 204 = HttpStatus 204 "No Content"
  toEnum 205 = HttpStatus 205 "Reset Content"
  toEnum 206 = HttpStatus 206 "Partial Content"
  toEnum 207 = HttpStatus 207 "Multi-Status"
  toEnum 208 = HttpStatus 208 "Already Reported"
  toEnum 226 = HttpStatus 226 "IM Used"

  toEnum 300 = HttpStatus 300 "Multiple Choices"
  toEnum 301 = HttpStatus 301 "Moved Permanently"
  toEnum 302 = HttpStatus 302 "Found"
  toEnum 303 = HttpStatus 303 "See Other"
  toEnum 304 = HttpStatus 304 "Not Modified"
  toEnum 305 = HttpStatus 305 "Use Proxy"
  toEnum 307 = HttpStatus 307 "Temporary Redirect"
  toEnum 308 = HttpStatus 308 "Permanent Redirect"

  toEnum 400 = HttpStatus 400 "Bad Request"
  toEnum 401 = HttpStatus 401 "Unauthorized"
  toEnum 402 = HttpStatus 402 "Payment Required"
  toEnum 403 = HttpStatus 403 "Forbidden"
  toEnum 404 = HttpStatus 404 "Not Found"
  toEnum 405 = HttpStatus 405 "Method Not Allowed"
  toEnum 406 = HttpStatus 406 "Not Acceptable"
  toEnum 407 = HttpStatus 407 "Proxy Authentication Required"
  toEnum 408 = HttpStatus 408 "Request Timeout"
  toEnum 409 = HttpStatus 409 "Conflict"
  toEnum 410 = HttpStatus 410 "Gone"
  toEnum 411 = HttpStatus 411 "Length Required"
  toEnum 412 = HttpStatus 412 "Precondition Failed"
  toEnum 413 = HttpStatus 413 "Payload Too Large"
  toEnum 414 = HttpStatus 414 "Request-URI Too Long"
  toEnum 415 = HttpStatus 415 "Unsupported Media Type"
  toEnum 416 = HttpStatus 416 "Requested Range Not Satisfiable"
  toEnum 417 = HttpStatus 417 "Expectation Failed"
  toEnum 418 = HttpStatus 418 "I'm a teapot"
  toEnum 421 = HttpStatus 421 "Misdirected Request"
  toEnum 422 = HttpStatus 422 "Unprocessable Entity"
  toEnum 423 = HttpStatus 423 "Locked"
  toEnum 424 = HttpStatus 424 "Failed Dependency"
  toEnum 426 = HttpStatus 426 "Upgrade Required"
  toEnum 428 = HttpStatus 428 "Precondition Required"
  toEnum 429 = HttpStatus 429 "Too Many Requests"
  toEnum 431 = HttpStatus 431 "Request Header Fields Too Large"
  toEnum 444 = HttpStatus 444 "Connection Closed Without Response"
  toEnum 451 = HttpStatus 451 "Unavailable For Legal Reasons"
  toEnum 499 = HttpStatus 499 "Client Closed Request"

  toEnum 500 = HttpStatus 500 "Internal Server Error"
  toEnum 501 = HttpStatus 501 "Not Implemented"
  toEnum 502 = HttpStatus 502 "Bad Gateway"
  toEnum 503 = HttpStatus 503 "Service Unavailable"
  toEnum 504 = HttpStatus 504 "Gateway Timeout"
  toEnum 505 = HttpStatus 505 "HTTP Version Not Supported"
  toEnum 506 = HttpStatus 506 "Variant Also Negotiates"
  toEnum 507 = HttpStatus 507 "Insufficient Storage"
  toEnum 508 = HttpStatus 508 "Loop Detected"
  toEnum 510 = HttpStatus 510 "Not Extended"
  toEnum 511 = HttpStatus 511 "Network Authentication Required"
  toEnum 599 = HttpStatus 599 "Network Connect Timeout Error"

getStatusCode :: HttpStatus -> Int
getStatusCode (HttpStatus c m) = c

constructStatus :: Int -> HttpStatus
constructStatus = toEnum

newtype HttpPath = HttpPath ByteString
    deriving (Show, Eq)

newtype HttpVersion = HttpVersion (ByteString, ByteString)
    deriving (Show, Eq)

newtype HttpHeader = HttpHeader (ByteString, ByteString)
    deriving (Show, Eq)

type HttpHeaders = [HttpHeader]

newtype HttpBody = HttpBody ByteString
    deriving (Show, Eq)

--
-- Request specific
--

data HttpRequestHead = HttpRequestHead HttpMethod HttpPath HttpVersion HttpHeaders
    deriving (Show, Eq)

data HttpRequest = HttpRequest HttpRequestHead HttpBody
    deriving (Show, Eq)

--
-- Response specific
--
--
data HttpResponseHead = HttpResponseHead HttpVersion HttpStatus HttpHeaders
    deriving (Show, Eq)

data HttpResponse = HttpResponse HttpResponseHead HttpBody
    deriving (Show, Eq)
