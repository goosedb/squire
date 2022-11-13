{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sql.Table.Columns where

import qualified GHC.Base as Nel
import Sql.Table.TableInfo (TableInfo)
import Sql.Types (SqlColumn)

class TableInfo ty => TableColumns ty where
  columns :: Nel.NonEmpty SqlColumn
  columnsNumber :: Int
  columnsNumber = length (columns @ty)
