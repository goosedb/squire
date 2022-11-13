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
import Sql.Table.Columns.TH (makeGenericColumns, (~>))
import Sql.Table.TableCodec (TableCodec)
import Sql.Table.TableInfo (TableInfo (..))
import Sql.Types (MaybeRow, RetRow, Row)
import Database.SQLite.Simple (open)
import Backend (Conn(SqliteConn), createTable, Backend (..), Insert (..))
import qualified Sql.Update as Q
import Sql.Update ((=:))

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
  type ID User = ID' User

  uniqueColumns UserUniqueName = #name :| []
data Pet = Pet
  { owner :: ID User
  , name :: String
  }
  deriving (Generic, Show,TableCodec)

instance TableInfo Pet where
  type Unique Pet = Void
  tableName = "pets"
  type ID Pet = ID' Pet

query :: Q.Query [RetRow (Row User :*: MaybeRow Pet)]
query = do
  user <- Q.from @User
  pet <- Q.leftJoin @Pet \pet -> #owner @? pet ==. Q.just (eid @. user)

  Q.orderBy [Q.desc $ #age @. user]
  Q.where' (#age @. user >. Q.val 18)

  Q.many (user :*: pet)

makeGenericColumns ''User ["age" ~> "user_age"]

makeGenericColumns ''Pet ["owner" ~> "pet_owner", "name" ~> "pet_name"]


someFunc :: IO ()
someFunc = do
  conn <- SqliteConn <$> open ":memory:"

  createTable @User conn
  createTable @Pet conn

  uId <- runInsert conn (InsertId (User "lol" 5))
  _ <- runInsert conn (InsertId (Pet uId "heh"))
  print uId

  r1 <- runQuery conn do
    u <- Q.from @User 
    p <- Q.leftJoin @Pet \p -> #owner @? p ==. Q.just (eid @. u)
    Q.where' (Q.eid @. u ==. Q.val uId)
    Q.one (p :*: u) 
  print r1

  runUpdate conn $ Q.update @Pet \_ -> Q.set [#name =: Q.val @String "mda"]  

  r2 <- runQuery conn do
    u <- Q.from @User 
    p <- Q.innerJoin @Pet \p -> #owner @. p ==. eid @. u
    Q.where' (Q.eid @. u ==. Q.val uId)
    Q.one (p :*: u) 

  _ <- runInsert conn (InsertId (User "kyk" 18))
  _ <- runInsert conn (InsertId (User "quz" 21))

  print r2

  r3 <- runQuery conn do
    u <- Q.from @User 
    p <- Q.leftJoin @Pet \p -> #owner @? p ==. Q.just (eid @. u)
    Q.many (u :*: p) 
  print r3


