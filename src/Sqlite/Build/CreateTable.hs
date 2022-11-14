{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sqlite.Build.CreateTable where

import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Sql.SqlType (SqlType (..))
import Sql.Table (IDDefinition (..), IsTable)
import Sql.Table.Columns (TableColumns (..))
import Sql.Table.TableInfo (TableInfo (..), fullTableNameQ)
import Sql.Types
    ( ColumnName(..),
      IDReference(IDReference, referenceColumn, referenceTable),
      SqlColumn(SqlColumn),
      TableName(TableName),
      UniqueColumn(UniqueColumn) )
import Data.Universe (Universe(universe))
import qualified Data.List.NonEmpty as Nel

createTable :: forall ty. (IDDefinition (ID ty), IsTable ty) => BS.Builder
createTable = t
  where
    t = "CREATE TABLE " <> coerce (fullTableNameQ @ty) <> " (" <> fold (intersperse ", " formattedColumns) <> ");"
    formattedColumns = case columns @ty of
      idColumn :| otherColumns -> formatIdColumn idColumn : (formatColumn <$> otherColumns) 
          <> mapMaybe formatReferences otherColumns 
          <> map formatUniques (universe @(Unique ty))
    formatColumn =
      \case
        SqlColumn (ColumnName n) ty _ ->
          let fTy = formattedType ty
           in n <> " " <> fTy

    formatReferences =
      \case
        SqlColumn (ColumnName n) _ (Just IDReference{..}) ->
          Just $ "FOREIGN KEY(" <> n <> ") REFERENCES " <> coerce referenceTable <> "(" <> coerce referenceColumn <> ")"
        _ -> Nothing

    formatUniques = \u -> 
      let cols = uniqueColumns @ty u 
      in "UNIQUE(" <> fold (intersperse ", "  $ Nel.toList $ (\(UniqueColumn cn _) -> coerce cn) <$> cols) <> ")"

    formatIdColumn (SqlColumn n _ _) = coerce n <> " " <> idDefinition @(ID ty)
    formattedType ty = case ty of
      SqlText -> "TEXT"
      SqlBytes -> "BLOB"
      SqlInt -> "INTEGER"
      SqlFloat -> "REAL"
      SqlBool -> "INTEGER"
      SqlAny -> error ""
      SqlNullable st -> formattedType st <> " NULLABLE"
