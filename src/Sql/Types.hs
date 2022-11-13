{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Sql.Types where

import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.String (IsString)
import Sql.SqlType (SqlType)

newtype Row a = Row TableName

newtype MaybeRow a = MaybeRow TableName

data RetRow a = RetRow
  deriving (Show)

newtype TableName = TableName BS.Builder
  deriving newtype (IsString, Semigroup, Monoid)

newtype SchemaName = SchemaName BS.Builder
  deriving newtype (IsString, Semigroup, Monoid)

newtype ColumnName = ColumnName BS.Builder
  deriving newtype (IsString, Semigroup, Monoid)

newtype TypedColumn ty a = TypedColumn ColumnName

newtype UniqueColumn ty = UniqueColumn ColumnName

unique :: forall a ty. TypedColumn ty a -> UniqueColumn ty
unique = coerce

data SqlColumn = SqlColumn
  { sqlColumnName :: ColumnName
  , sqlColumnType :: SqlType
  , sqlColumnIDReference :: Maybe IDReference
  }

data IDReference = IDReference
  { referenceTable :: TableName
  , referenceColumn :: ColumnName
  }
