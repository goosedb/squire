{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
import Sqlite.Build.Query (buildExpr, buildFrom, buildWhere)
import Sqlite.Build.Types (BuildState)

buildUpdate :: Update ty () -> State BuildState BS.Builder
buildUpdate (runUpdate 0 -> UpdateState{..}) = do
  let updatedTables = fold $ intersperse "," $ map (\case SetField (TableName tn) _ _ -> "\"" <> tn <> "\"") updateFields_
  set_ <-
    fold . intersperse (", " :: BS.Builder) <$> forM
      updateFields_
      \case
        SetField _ (ColumnName cn) (SqlExpr se) -> do
          e' <- buildExpr se
          pure $ "\"" <> cn <> "\" = " <> e'
  fromTables_ <- case fromClause_ of
    [] -> pure ""
    a -> (\x -> "\nFROM ( SELECT " <> x <> ")") <$> buildFrom a
  where_ <- buildWhere $ coerce whereClause_
  pure $ "UPDATE " <> updatedTables <> " SET " <> set_ <> " " <> fromTables_ <> " " <> where_
