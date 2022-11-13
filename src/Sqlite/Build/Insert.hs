{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Sqlite.Build.Insert where

import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.List (intersperse)
import qualified Data.List.NonEmpty as Nel
import Database.SQLite.Simple (SQLData)
import Sql.Table (IsTable)
import Sql.Table.Columns (TableColumns (..))
import Sql.Table.TableCodec (encode)
import Sql.Table.TableInfo (fullTableName)
import Sql.Types (ColumnName (..), SqlColumn (..), TableName (..))
import Sqlite.Build.Common (transformTo)

buildInsert :: forall ty. IsTable ty => ty -> (BS.Builder, [SQLData])
buildInsert ty =
  let q =
        "INSERT INTO "
          <> coerce (fullTableName @ty)
          <> "("
          <> fold (intersperse "," $ map (\(SqlColumn n _ _) -> coerce n) $ Nel.tail (columns @ty))
          <> ") VALUES ("
          <> fold (intersperse "," $ map (const "?") $ Nel.tail (columns @ty))
          <> ");"
   in (q, transformTo <$> encode ty)
