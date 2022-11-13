{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sql.SqlExpr.Internal where

import qualified Data.ByteString.Builder as BS
import Sql.SqlValue (SqlValue)
import Sql.Types (ColumnName, RetRow, TableName)

data SqlExpr_ q a where
  BoolOp :: BS.Builder -> SqlExpr_ q a -> SqlExpr_ q a -> SqlExpr_ q Bool
  BinOp :: BS.Builder -> SqlExpr_ q a -> SqlExpr_ q a -> SqlExpr_ q a
  In :: SqlExpr_ q a -> [SqlExpr_ q a] -> SqlExpr_ q Bool
  Column :: TableName -> ColumnName -> SqlExpr_ q a
  Value :: SqlValue -> SqlExpr_ q b
  CastNullable :: SqlExpr_ q a -> SqlExpr_ q (Maybe a)
  CastMandatory :: SqlExpr_ q (Maybe a) -> SqlExpr_ q a
  Function :: BS.Builder -> [SqlExpr_ q a] -> SqlExpr_ q b
  Exists :: q (f (RetRow a)) -> SqlExpr_ q Bool
  Raw :: BS.Builder -> SqlExpr_ q a
