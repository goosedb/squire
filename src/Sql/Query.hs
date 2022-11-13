{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Sql.Query where

import Control.Monad.State.Strict (execState, gets, modify)
import qualified Data.ByteString.Builder as BS
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.String (IsString (..))
import Sql.Query.Types (JoinType (..), Order (..), OrderExpr (..), Query (..), QueryState (..), ReturnExpr (..), SelectTable (..))
import Sql.SqlExpr.Internal (SqlExpr_ (..))
import Sql.SqlType (SqlType (..))
import Sql.SqlValue (IsSqlValue (..), Parser, skipN)
import Sql.Table (Entity (..), IsTable)
import Sql.Table.Columns (TableColumns (..))
import Sql.Table.TableCodec (TableCodec (..))
import Sql.Table.TableInfo (TableInfo (..), fullTableName)
import Sql.Types
  ( MaybeRow (..)
  , RetRow
  , Row (..)
  , SqlColumn (..)
  , TypedColumn (..)
  )

newtype SqlExpr a = SqlExpr (SqlExpr_ Query a)

val :: IsSqlValue a => a -> SqlExpr a
val = SqlExpr . Value . toSqlValue

valList :: forall a. IsSqlValue a => [a] -> [SqlExpr a]
valList = map (SqlExpr . Value . toSqlValue)

boolOp :: BS.Builder -> SqlExpr a -> SqlExpr a -> SqlExpr Bool
boolOp f (SqlExpr a) (SqlExpr b) = SqlExpr (BoolOp f a b)

binOp :: BS.Builder -> SqlExpr a -> SqlExpr a -> SqlExpr a
binOp f (SqlExpr a) (SqlExpr b) = SqlExpr (BinOp f a b)

(==.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(==.) = boolOp "="

infix 4 ==.

(/=.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(/=.) = boolOp "<>"

infix 4 /=.

(>.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(>.) = boolOp ">"

in' :: SqlExpr a -> [SqlExpr a] -> SqlExpr Bool
in' (SqlExpr a) b = SqlExpr $ In a (coerce b)

infix 4 >.

(<.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(<.) = boolOp "<"

infix 4 <.

(>=.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(>=.) = boolOp ">="

infix 4 >=.

(<=.) :: SqlExpr a -> SqlExpr a -> SqlExpr Bool
(<=.) = boolOp "<="

infix 4 <=.

(&&.) :: SqlExpr Bool -> SqlExpr Bool -> SqlExpr Bool
(&&.) = boolOp "and"

infixr 3 &&.

(||.) :: SqlExpr Bool -> SqlExpr Bool -> SqlExpr Bool
(||.) = boolOp "or"

infixr 2 ||.

(*.) :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a -> SqlExpr a
(*.) = binOp "*"

infixl 7 *.

(/.) :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a -> SqlExpr a
(/.) = binOp "/"

infixl 7 /.

(+.) :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a -> SqlExpr a
(+.) = binOp "+"

infixl 6 +.

(-.) :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a -> SqlExpr a
(-.) = binOp "-"

infixl 6 -.

(@.) :: TypedColumn ty a -> Row ty -> SqlExpr a
(@.) (TypedColumn n) (Row alias) = SqlExpr $ Column alias n

(@?) :: TypedColumn ty a -> MaybeRow ty -> SqlExpr (Maybe a)
(@?) (TypedColumn n) (MaybeRow alias) = SqlExpr $ Column alias n

infixl 8 @., @?

just :: SqlExpr a -> SqlExpr (Maybe a)
just = SqlExpr . CastNullable . coerce

coalesce :: [SqlExpr (Maybe a)] -> SqlExpr (Maybe a)
coalesce = SqlExpr . Function "coalesce" . coerce

orElse :: SqlExpr (Maybe a) -> SqlExpr a -> SqlExpr a
orElse a b = SqlExpr $ CastMandatory $ coerce $ coalesce [a, just b]

count :: Num b => SqlExpr a -> SqlExpr b
count = SqlExpr . Function "count" . pure . coerce

class SqlNumber (a :: SqlType)
instance SqlNumber 'SqlFloat
instance SqlNumber 'SqlInt
instance SqlNumber a => SqlNumber ( 'SqlNullable a)

avg :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a
avg = SqlExpr . Function "avg" . pure . coerce

sum :: SqlNumber (SqlTypeOf a) => SqlExpr a -> SqlExpr a
sum = SqlExpr . Function "sum" . pure . coerce

runQuery :: Int -> Query (f (RetRow a)) -> QueryState
runQuery i q = execState (getQueryState q) $ QueryState [] Nothing [] [] Nothing [] Nothing i

from :: forall a. IsTable a => Query (Row a)
from = Query do
  i <- gets index
  let alias = tableName @a <> "_" <> fromString (show i)
  modify \QueryState{..} ->
    QueryState
      { from_ =
          from_
            <> [SelectTable (fullTableName @a) alias Nothing]
      , index = index + 1
      , ..
      }
  pure $ Row alias

fromMaybe :: forall a. IsTable a => Query (MaybeRow a)
fromMaybe = Query do
  i <- gets index
  let alias = tableName @a <> "_" <> fromString (show i)
  modify \QueryState{..} ->
    QueryState
      { from_ =
          from_
            <> [SelectTable (fullTableName @a) alias Nothing]
      , index = index + 1
      , ..
      }
  pure $ MaybeRow alias

fullOuterJoin :: forall a. IsTable a => (MaybeRow a -> SqlExpr Bool) -> Query (MaybeRow a)
fullOuterJoin f = Query do
  i <- gets index
  let alias = tableName @a <> "_" <> fromString (show i)
  let row = MaybeRow alias
  modify \QueryState{..} ->
    QueryState
      { from_ =
          from_
            <> [SelectTable (fullTableName @a) alias $ Just (FullOuter, coerce f row)]
      , index = index + 1
      , ..
      }
  pure row

innerJoin :: forall a. IsTable a => (Row a -> SqlExpr Bool) -> Query (Row a)
innerJoin f = Query do
  i <- gets index
  let alias = tableName @a <> "_" <> fromString (show i)
  let row = Row alias
  modify \QueryState{..} ->
    QueryState
      { from_ =
          from_
            <> [SelectTable (fullTableName @a) alias $ Just (Inner, coerce f row)]
      , index = index + 1
      , ..
      }
  pure row

leftJoin :: forall a. IsTable a => (MaybeRow a -> SqlExpr Bool) -> Query (MaybeRow a)
leftJoin f = Query do
  i <- gets index
  let alias = tableName @a <> "_" <> fromString (show i)
  let row = MaybeRow alias
  modify \QueryState{..} ->
    QueryState
      { from_ =
          from_
            <> [SelectTable (fullTableName @a) alias $ Just (LeftOuter, coerce f row)]
      , ..
      }
  pure row

class Where m where
  where' :: SqlExpr Bool -> m ()

instance Where Query where
  where' cond = Query do
    modify \QueryState{..} ->
      QueryState{whereClause_ = Just $ coerce cond, ..}

orderBy :: [OrderExpr] -> Query ()
orderBy e = Query do
  modify \QueryState{..} ->
    QueryState{orderBy_ = orderBy_ <> e, ..}

desc :: SqlExpr a -> OrderExpr
desc = flip OrderExpr Desc . coerce

asc :: SqlExpr a -> OrderExpr
asc = flip OrderExpr Asc . coerce

limit :: Int -> Query ()
limit l = Query do
  modify \QueryState{..} ->
    QueryState{limit_ = Just l, ..}

data a :*: b = a :*: b
  deriving (Show)

infixr 9 :*:

class Return a where
  ret :: a -> [ReturnExpr]

instance (Return b, Return a) => Return (a :*: b) where
  ret (a :*: b) = ret a <> ret b

instance IsTable a => Return (MaybeRow a) where
  ret (MaybeRow tn) = toList (columns @a) <&> (\case SqlColumn n _ _ -> ReturnExpr $ Column tn n)

instance IsTable a => Return (Row a) where
  ret (Row tn) = toList (columns @a) <&> (\case SqlColumn n _ _ -> ReturnExpr $ Column tn n)

instance Return (SqlExpr a) where
  ret a = [ReturnExpr $ coerce a]

many :: Return a => a -> Query [RetRow a]
many a = Query do
  modify \QueryState{..} -> QueryState{returnVals_ = ret a, ..}
  pure []

one :: Return a => a -> Query (Maybe (RetRow a))
one a = do
  limit 1
  Query do
    modify \QueryState{..} -> QueryState{returnVals_ = ret a, ..}
  pure Nothing

groupBy :: (Return a, Return b) => a -> Maybe b -> Maybe (SqlExpr Bool) -> Query [RetRow (a :*: b)]
groupBy a b having = do
  Query do
    modify \QueryState{..} ->
      QueryState
        { having_ = coerce having
        , groupVals_ = ret a
        , returnVals_ = ret a <> maybe [] ret b
        , ..
        }
  pure []

class Parsed a where
  type Result a :: *
  parseRow :: Parser (Result a)

instance (Parsed b, Parsed a) => Parsed (a :*: b) where
  type Result (a :*: b) = (Result a :*: Result b)
  parseRow = (:*:) <$> parseRow @a <*> parseRow @b

instance IsSqlValue a => Parsed (SqlExpr a) where
  type Result (SqlExpr a) = a
  parseRow = fromSqlValue

instance Parsed a => Parsed (RetRow a) where
  type Result (RetRow a) = Result a
  parseRow = parseRow @a

instance IsTable a => Parsed (Row a) where
  type Result (Row a) = Entity a
  parseRow = Entity <$> fromSqlValue <*> decode

instance IsTable a => Parsed (MaybeRow a) where
  type Result (MaybeRow a) = Maybe (Entity a)

  parseRow = do
    mbId <- fromSqlValue
    case mbId of
      Just i -> Just . Entity i <$> decode
      Nothing -> Nothing <$ skipN (columnsNumber @a - 1)

exists :: Query () -> SqlExpr Bool
exists q = SqlExpr $ Exists do
  q >> many (SqlExpr $ Raw @Query @Int "1")

eid :: forall ty. IsTable ty => TypedColumn ty (ID ty)
eid = TypedColumn (idColumnName @ty)
