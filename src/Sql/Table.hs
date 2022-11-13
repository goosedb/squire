{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sql.Table where

import qualified Data.ByteString.Builder as BS
import Data.Int (Int64)
import Sql.SqlType (SqlType (..))
import Sql.SqlValue (IsSqlValue (..), SqlValue (..))
import Sql.Table.Columns (TableColumns)
import Sql.Table.TableCodec (TableCodec)
import Sql.Table.TableInfo (TableInfo (..))
import Sql.Types (ColumnName (..), TableName)

newtype ID' a = ID' Int64
  deriving (Show, Eq)

class IDDefinition id where
  idDefinition :: BS.Builder

instance IDDefinition (ID' a) where
  idDefinition = "INTEGER PRIMARY KEY"
instance IsSqlValue (ID' a) where
  type SqlTypeOf (ID' a) = 'SqlInt
  toSqlValue (ID' in') = SqlIntValue in'
  fromSqlValue = ID' <$> fromSqlValue

data Entity a = Entity
  { entID :: ID a
  , ent :: a
  }

deriving instance (Show (ID a), Show a) => Show (Entity a)

type IsTable ty = (TableColumns ty, TableCodec ty, TableInfo ty)
