{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.FormatQueryType where

import Prelude

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.QueryType
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.Types (Oid(..), Only(..), Values(..))
import Foreign.C.Types (CUInt)

data FormattedQueryType = FormattedQueryType
  { inputTypeNames :: [Text]
  , outputTypeNames :: [(Text, Text)]
  } deriving (Show)

formatType :: PGSimple.Connection -> QueryType -> IO FormattedQueryType
formatType conn queryType = do
  inputTypeNames <- getInputTypeNames conn (inputTypes queryType)
  outputTypeNames <- getOutputTypeNames conn (outputTypes queryType)
  pure $ FormattedQueryType
    { inputTypeNames = inputTypeNames
    , outputTypeNames = outputTypeNames
    }

getInputTypeNames :: PGSimple.Connection -> [CUInt] -> IO [Text]
getInputTypeNames conn types = map head <$> PGSimple.query conn template values
  where
    withOrdering =
      zipWith (\i n -> (i, Oid n)) [0 .. length types] types
    template =
      "SELECT pgt.typname \
      \FROM pg_catalog.pg_type pgt \
      \JOIN (?) type_oids(ordering, oid) \
      \ON pgt.oid = type_oids.oid \
      \ORDER BY type_oids.ordering ASC"
    values = Only $ Values ["int4", "oid"] withOrdering

getOutputTypeNames
  :: PGSimple.Connection
  -> [(ByteString, CUInt)]
  -> IO [(Text, Text)]
getOutputTypeNames conn types = PGSimple.query conn template values
  where
    template =
      "SELECT type_oids.colname, pgt.typname \
      \FROM pg_catalog.pg_type pgt \
      \JOIN (?) type_oids(colname, oid) \
      \ON pgt.oid = type_oids.oid "
    values = Only $ Values ["text", "oid"] $ map (\p -> (fst p, Oid (snd p))) types
