{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

module Sqlite.Build.Update where

import Control.Monad (forM)
import Control.Monad.State.Strict (State)
import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.List (intersperse)
import Sql.Query (SqlExpr (..))
import Sql.Types (ColumnName (ColumnName), TableName (..))
import Sql.Update (SetField (..), Update, UpdateState (..), runUpdate)
import Sqlite.Build.Query (buildExpr, buildWhere)
import Sqlite.Build.Types (BuildState)
import Sql.Table.TableInfo (fullTableNameQ)
import Sql.Table (IsTable)

buildUpdate :: forall ty. IsTable ty => Update ty () -> State BuildState BS.Builder
buildUpdate (runUpdate 0 -> UpdateState{..}) = do
  let updatedTables = coerce (fullTableNameQ @ty)
  let ?tablePrefix = False
  set_ <-
    fold . intersperse (", " :: BS.Builder) <$> forM
      updateFields_
      \case
        SetField (ColumnName cn) (SqlExpr se) -> do
          e' <- buildExpr se
          pure $ "\"" <> cn <> "\" = " <> e'
  where_ <- buildWhere $ coerce whereClause_
  pure $ "UPDATE " <> updatedTables <> " SET " <> set_ <> " "  <> " " <> where_
