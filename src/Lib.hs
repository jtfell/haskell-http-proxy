module Lib
  (
    getRequestPath,
    getRequestMethod,
    constructNewRequest
  ) where

--
-- Helper functions for parsing HTTP requests
--
getRequestPath :: String -> String
getRequestPath req = words req !! 1

getRequestMethod :: String -> String
getRequestMethod = head . words

constructNewRequest :: String -> String -> String
constructNewRequest req host = getRequestMethod req ++ " " ++ host ++ getRequestPath req
