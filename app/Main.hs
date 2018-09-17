{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Database.PostgreSQL.FormatQueryType
import Database.PostgreSQL.QueryType
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.Simple as PGSimple
import Data.ByteString (ByteString)
import Data.Text.Encoding
import qualified Data.Text as Text

main :: IO ()
main = do
  connectInfo <- getConnectInfo
  libpqConnection <- LibPQ.connectdb $ PGSimple.postgreSQLConnectionString connectInfo
  query <- getQuery
  queryType <- getQueryTypes libpqConnection "test_statement2" query
  pgSimpleConnection <- PGSimple.connect connectInfo
  formatted <- formatType pgSimpleConnection queryType
  putStrLn $ show formatted

getConnectInfo :: IO PGSimple.ConnectInfo
getConnectInfo = do
  putStrLn "DBName:"
  dbname <- getLine
  putStrLn "User:"
  user <- getLine
  pure $ PGSimple.defaultConnectInfo
    { PGSimple.connectDatabase = dbname
    , PGSimple.connectUser = user
    }

getQuery :: IO ByteString
getQuery = do
  putStrLn "Query:"
  encodeUtf8 . Text.pack <$> getLine
