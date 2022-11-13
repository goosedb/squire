{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sqlite.Build.Query where

import Control.Monad (forM)
import Control.Monad.State.Strict (State, gets, modify')
import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.String (IsString (..))
import Database.SQLite.Simple (SQLData (..))
import Sql.Query (SqlExpr (..), runQuery)
import Sql.Query.Types (JoinType (..), Order (..), OrderExpr (OrderExpr), Query, QueryState (..), ReturnExpr (..), SelectTable (..))
import Sql.SqlExpr.Internal (SqlExpr_ (..))
import Sql.SqlValue (SqlValue (..))
import Sql.Types (ColumnName (..), RetRow, TableName (..))
import Sqlite.Build.Types (BuildState (..))

buildQuery' :: QueryState -> State BuildState BS.Builder
buildQuery' QueryState{..} = do
  modify' \BuildState{..} -> BuildState{buildIndex = index, ..}
  select_ <- buildSelectVals returnVals_
  fromVals_ <- ("\nFROM " <>) <$> buildFrom from_
  where_ <- buildWhere $ coerce whereClause_
  group_ <- buildGroupBy groupVals_ $ coerce having_
  order_ <- buildOrder orderBy_
  let lim_ = buildLimit limit_
  modify' \BuildState{..} -> BuildState{vals = reverse vals, ..}
  pure $ select_ <> " " <> fromVals_ <> " " <> where_ <> " " <> group_ <> " " <> order_ <> " " <> lim_

buildQuery :: Query (f (RetRow a)) -> State BuildState BS.Builder
buildQuery = buildQuery' . runQuery 0

buildFrom :: [SelectTable] -> State BuildState BS.Builder
buildFrom fromTables = do
  tables <- forM
    fromTables
    \case
      SelectTable (TableName name) (TableName alias) j -> case j of
        Nothing -> pure $ "\"" <> name <> "\" AS " <> alias
        Just (jt, cl) -> do
          cl' <- buildExpr cl
          pure case jt of
            Inner -> " INNER JOIN \"" <> name <> "\" AS " <> alias <> " ON " <> cl'
            LeftOuter -> " LEFT OUTER JOIN \"" <> name <> "\" AS " <> alias <> " ON " <> cl'
            FullOuter -> " FULL OUTER JOIN \"" <> name <> "\" AS " <> alias <> " ON " <> cl'
  pure $ fold tables

buildSelectVals :: [ReturnExpr] -> State BuildState BS.Builder
buildSelectVals returnValues = do
  fields <- buildFields returnValues
  pure $ "SELECT " <> fields

buildFields :: [ReturnExpr] -> State BuildState BS.Builder
buildFields returnValues = do
  built <- forM returnValues \(ReturnExpr s) -> buildExpr s
  pure . fold $ intersperse "," built

buildLimit :: Maybe Int -> BS.Builder
buildLimit = maybe "" (("\nLIMIT " <>) . fromString . show)

buildOrder :: [OrderExpr] -> State BuildState BS.Builder
buildOrder = \case
  [] -> pure ""
  list -> do
    builtList <- fold . intersperse "," <$> go list
    pure $ "\nORDER BY " <> builtList
  where
    go [] = pure []
    go (OrderExpr e o : rest) = case o of
      Desc -> (:) <$> (buildExpr e <&> (<> " DESC")) <*> go rest
      Asc -> (:) <$> (buildExpr e <&> (<> " ASC")) <*> go rest

buildWhere :: Maybe (SqlExpr Bool) -> State BuildState BS.Builder
buildWhere = \case
  Just a -> ("\nWHERE " <>) <$> buildExpr (coerce a)
  Nothing -> pure ""

buildGroupBy :: [ReturnExpr] -> Maybe (SqlExpr Bool) -> State BuildState BS.Builder
buildGroupBy [] _ = pure ""
buildGroupBy g h = do
  f <- buildFields g
  h' <- traverse buildExpr (coerce h)
  pure $ "\nGROUP BY " <> f <> maybe "" (" HAVING " <>) h'

buildExpr :: SqlExpr_ Query a -> State BuildState BS.Builder
buildExpr = \case
  BoolOp s l r -> do
    l' <- buildExpr l
    r' <- buildExpr r
    pure $ "(" <> l' <> " " <> s <> " " <> r' <> ")"
  BinOp s l r -> do
    l' <- buildExpr l
    r' <- buildExpr r
    pure $ "(" <> l' <> " " <> s <> " " <> r' <> ")"
  Column (TableName tn) (ColumnName cn) -> pure $ tn <> ".\"" <> cn <> "\""
  Value sv -> do
    modify' \BuildState{..} -> BuildState{vals = sqlData sv : vals, ..}
    pure "?"
  CastMandatory se -> buildExpr se
  CastNullable se -> buildExpr se
  Function s ses -> do
    es <- traverse buildExpr ses
    pure $ s <> "(" <> fold (intersperse "," es) <> ")"
  Exists q -> do
    i <- gets buildIndex
    q' <- buildQuery' (runQuery i q)
    pure $ "EXISTS (" <> q' <> ")"
  Raw t -> pure t
  In v list -> do
    v' <- buildExpr v
    list' <- traverse buildExpr list
    pure $ v' <> " IN (" <> fold (intersperse "," list') <> ")"
  where
    sqlData :: SqlValue -> SQLData
    sqlData v = case v of
      SqlTextValue txt -> SQLText txt
      SqlBytesValue bs -> SQLBlob bs
      SqlIntValue in' -> SQLInteger in'
      SqlFloatValue x -> SQLFloat x
      SqlBoolValue b -> SQLInteger (if b then 1 else 0)
      SqlNull -> SQLNull
