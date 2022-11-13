{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sql.Query.Types where

import Control.Monad.State.Strict (State)
import Sql.SqlExpr.Internal (SqlExpr_)
import Sql.Types (TableName)

data JoinType = Inner | LeftOuter | FullOuter
  deriving (Show)

type OnClause = SqlExpr_ Query Bool

type WhereClause = SqlExpr_ Query Bool

data SelectTable = SelectTable TableName TableName (Maybe (JoinType, OnClause))

data ReturnExpr where
  ReturnExpr :: SqlExpr_ Query a -> ReturnExpr

data Order = Desc | Asc
  deriving (Show)

data OrderExpr where
  OrderExpr :: SqlExpr_ Query a -> Order -> OrderExpr

data QueryState = QueryState
  { from_ :: [SelectTable]
  , whereClause_ :: Maybe WhereClause
  , returnVals_ :: [ReturnExpr]
  , orderBy_ :: [OrderExpr]
  , limit_ :: Maybe Int
  , groupVals_ :: [ReturnExpr]
  , having_ :: Maybe WhereClause
  , index :: Int
  }

newtype Query a = Query {getQueryState :: State QueryState a}
  deriving newtype (Functor, Applicative, Monad)
