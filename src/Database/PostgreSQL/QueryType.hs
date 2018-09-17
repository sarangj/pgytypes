{-# LANGUAGE DeriveGeneric #-}
module Database.PostgreSQL.QueryType where
import Prelude

import GHC.Generics

import Control.Exception (Exception, throwIO)
import Control.Monad (mapM)
import Control.Monad.Extra
import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Foreign.C.Types (CUInt)

type PreparedStatementName = ByteString

type Query = ByteString

data QueryType = QueryType
  { inputTypes :: [CUInt]
  , outputTypes :: [(ByteString, CUInt)]
  } deriving (Show)

getQueryTypes
  :: LibPQ.Connection
  -> PreparedStatementName
  -> Query
  -> IO QueryType
getQueryTypes connection statementName query = do
  prepared <- LibPQ.prepare connection statementName query Nothing
  putStrLn $ show query
  case prepared of
    Nothing -> throw "Failed to prepare statement"
    Just _ -> pure ()
  result <- LibPQ.describePrepared connection statementName
  case result of
    Nothing -> throw "Failed to describe prepared statement"
    Just r -> QueryType <$> getInputTypes r <*> getOutputTypes r

getInputTypes :: LibPQ.Result -> IO [CUInt]
getInputTypes result = do
  numParams <- LibPQ.nparams result
  mapM (fmap unOid . LibPQ.paramtype result) [0 .. numParams - 1]

getOutputTypes :: LibPQ.Result -> IO [(ByteString, CUInt)]
getOutputTypes result = do
  numFields <- (\n -> n-1) <$> LibPQ.nfields result
  fieldNames <- mapMaybeM (LibPQ.fname result) [0 .. numFields]
  fieldTypes <- mapM (fmap unOid . LibPQ.ftype result) [0 .. numFields]
  pure (zip fieldNames fieldTypes)

unOid :: LibPQ.Oid -> CUInt
unOid (LibPQ.Oid oid) = oid

newtype PGException = PGException String deriving (Generic)

instance Show PGException where
  show (PGException s) = "PG Exception: " ++ s

instance Exception PGException

throw :: String -> IO a
throw = throwIO . PGException
