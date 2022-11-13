{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sqlite.Build.DropTable where

import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Sql.Table (IsTable)
import Sql.Table.TableInfo (fullTableName)
import Sql.Types (TableName (..))

dropTable :: forall ty. IsTable ty => BS.Builder
dropTable = "DROP TABLE " <> coerce (fullTableName @ty) <> ";"
