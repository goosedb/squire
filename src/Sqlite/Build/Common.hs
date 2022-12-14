{-# LANGUAGE LambdaCase #-}

module Sqlite.Build.Common where

import qualified Database.SQLite.Simple as Sqlite
import Sql.SqlValue (SqlValue (..))

transformTo :: SqlValue -> Sqlite.SQLData
transformTo = \case
  SqlTextValue txt -> Sqlite.SQLText txt
  SqlBytesValue bs -> Sqlite.SQLBlob bs
  SqlIntValue in' -> Sqlite.SQLInteger in'
  SqlFloatValue x -> Sqlite.SQLFloat x
  SqlBoolValue b -> Sqlite.SQLInteger (if b then 1 else 0)
  SqlNull -> Sqlite.SQLNull

transformFrom :: Sqlite.SQLData -> SqlValue
transformFrom a = case a of
  Sqlite.SQLInteger in' -> SqlIntValue in'
  Sqlite.SQLFloat x -> SqlFloatValue x
  Sqlite.SQLText txt -> SqlTextValue txt
  Sqlite.SQLBlob bs -> SqlBytesValue bs
  Sqlite.SQLNull -> SqlNull
