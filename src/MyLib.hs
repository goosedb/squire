{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Universe (Universe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Sql.Query (eid, (==.), (>.), (@.), (@?), type (:*:) (..))
import qualified Sql.Query as Q
import qualified Sql.Query.Types as Q
import Sql.Table (ID')
import Sql.Table.Columns.TH ( makeGenericColumns, (~>) )
import Sql.Table.TableCodec (TableCodec)
import Sql.Table.TableInfo (TableInfo (..))
import Sql.Types (MaybeRow, RetRow, Row, unique)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data User = User
  { name :: String
  , age :: Int
  }
  deriving (Generic, Show, TableCodec)

data UserUnique = UserUniqueName
  deriving (Generic, Enum, Show, Bounded, Universe)

instance TableInfo User where
  type Unique User = UserUnique
  tableName = "users"
  schemaName = Just "main"
  type ID User = ID' User

  uniqueColumns UserUniqueName = unique #name :| []

makeGenericColumns ''User ["age" ~> "user_age"]
data Pet = Pet
  { owner :: ID User
  , name :: String
  }
  deriving (Generic, TableCodec)

makeGenericColumns ''Pet ["owner" ~> "pet_owner", "name" ~> "pet_name"]

instance TableInfo Pet where
  type Unique Pet = Void
  tableName = "pets"
  schemaName = Just "main"
  type ID Pet = ID' Pet

query :: Q.Query [RetRow (Row User :*: MaybeRow Pet)]
query = do
  user <- Q.from @User
  pet <- Q.leftJoin @Pet \pet -> #owner @? pet ==. Q.just (eid @. user)

  Q.orderBy [Q.desc $ #age @. user]
  Q.where' (#age @. user >. Q.val 18)

  Q.many (user :*: pet)
