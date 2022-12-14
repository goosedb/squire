{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Sql.Update where

import Control.Monad.State.Strict (State, execState, modify)
import Data.Coerce (coerce)
import Sql.Query (SqlExpr (..), Where (..))
import Sql.Query.Types (SelectTable (..))
import Sql.SqlExpr.Internal
  ( SqlExpr_ (..)
  )
import Sql.Table (IsTable)
import Sql.Table.TableInfo (TableInfo (..))
import Sql.Types (ColumnName, Row (..), TypedColumn (..))

data SetField ty where
  SetField :: ColumnName -> SqlExpr a -> SetField ty

data UpdateState ty = UpdateState
  { updateFields_ :: [SetField ty]
  , fromClause_ :: [SelectTable]
  , whereClause_ :: Maybe (SqlExpr Bool)
  , index :: Int
  }

(=:) :: forall a ty. TypedColumn ty a -> SqlExpr a -> SetField ty
(=:) (TypedColumn col) = SetField col

newtype Update ty a = Update {getUpdateState :: State (UpdateState ty) a}
  deriving newtype (Functor, Applicative, Monad)

runUpdate :: Int -> Update a () -> UpdateState a
runUpdate i = flip execState (UpdateState [] [] Nothing i) . getUpdateState

update :: forall a. IsTable a => (Row a -> Update a ()) -> Update a ()
update f = f (Row (tableName @a))

set :: [SetField a] -> Update a ()
set s = Update do 
  modify (\UpdateState {..} -> UpdateState { updateFields_ = updateFields_ <> s, ..})

instance Where (Update ty) where
  where' cond = Update do
    modify \UpdateState{..} ->
      UpdateState{whereClause_ = Just $ coerce cond, ..}

imSureItsNotNull :: SqlExpr (Maybe a) -> SqlExpr a
imSureItsNotNull (SqlExpr a) = SqlExpr $ CastMandatory a
