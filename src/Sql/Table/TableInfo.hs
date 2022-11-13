{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Sql.Table.TableInfo where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality (type (==))
import Data.Universe (Universe)
import Data.Void (Void, absurd)
import qualified GHC.TypeLits as TE
import Sql.SqlValue (IsSqlValue)
import Sql.Types (UniqueColumn)
import qualified Sql.Types as Types
import Types (IF)

type family HasUnique ty where
  HasUnique ty =
    IF
      (Unique ty == Void)
      ( TE.TypeError
          ( 'TE.Text "Type `" 'TE.:<>: 'TE.ShowType ty 'TE.:<>: 'TE.Text "` has no uniques, so can't be used it here.")
      )
      ()

class (Universe (Unique ty), IsSqlValue (ID ty)) => TableInfo ty where
  type ID ty :: *
  type Unique ty :: *
  schemaName :: Maybe Types.SchemaName
  schemaName = Nothing
  tableName :: Types.TableName
  idColumnName :: Types.ColumnName
  idColumnName = "id"

  uniqueColumns :: Unique ty -> NonEmpty (UniqueColumn ty)
  default uniqueColumns :: Unique ty ~ Void => Unique ty -> NonEmpty (UniqueColumn ty)
  uniqueColumns = absurd

fullTableName :: forall ty. TableInfo ty => Types.TableName
fullTableName = maybe "" (<> ".") (coerce $ schemaName @ty) <> tableName @ty
