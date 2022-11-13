{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Sqlite.Build.Delete where

import Control.Arrow (Arrow (..))
import Control.Monad.State.Strict (runState)
import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Database.SQLite.Simple (SQLData)
import Sql.Query (SqlExpr (..))
import Sql.Table (IsTable)
import Sql.Table.TableInfo (fullTableName)
import Sql.Types (TableName (..))
import Sqlite.Build.Query (buildExpr)
import Sqlite.Build.Types (BuildState (BuildState, vals))

buildDelete :: forall ty. IsTable ty => Maybe (SqlExpr Bool) -> (BS.Builder, [SQLData])
buildDelete expr =
  let q = "DELETE FROM " <> coerce (fullTableName @ty) <> maybe ";" (const (" WHERE " <> whereSegment <> ";")) expr
      (whereSegment, vals') = maybe ("", []) (second vals . flip runState (BuildState [] 0) . buildExpr) (coerce expr)
   in (q, vals')
