module Sqlite.Build.Types where

import Database.SQLite.Simple (SQLData)

data BuildState = BuildState
  { vals :: [SQLData]
  , buildIndex :: Int
  }
  deriving (Show)
