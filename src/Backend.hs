{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend where

import Control.Monad (forM, forM_)
import Control.Monad.State.Strict (runState)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Kind (Constraint)
import Data.List (intersperse)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as T
import Sql.SqlExpr.Internal ( SqlExpr_(Value) )
import Data.Type.Equality (type (==))
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.FromField ()
import qualified GHC.TypeLits as TE
import Sql.Query (Parsed (..), eid, from, one, val, where', (==.), (@.), (&&.), SqlExpr (..))
import Sql.Query.Types (Query)
import Sql.SqlValue (runEitherParser, IsSqlValue)
import Sql.Table (Entity, ID' (..), IsTable, IDDefinition)
import Sql.Table.Columns (TableColumns (..))
import Sql.Table.TableCodec (TableCodec (..))
import Sql.Table.TableInfo (HasUnique, TableInfo (..))
import Sql.Types (ColumnName (..), RetRow, SqlColumn (..), TableName (..), UniqueColumn (UniqueColumn), TypedColumn (TypedColumn), Row)
import Sql.Update (SetField, Update)
import qualified Sqlite.Build.Query as Lite
import qualified Sqlite.Build.Types as Lite
import qualified Sqlite.Build.Update as Lite
import Types (IF)
import Sqlite.Build.Common (transformFrom, transformTo)
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.List.NonEmpty as Nel
import qualified Data.Text as TS
import Data.Foldable (Foldable(fold))
import Data.Universe (Universe(universe))
import qualified Sql.Update as L
import qualified Sql.Query as Q
import qualified Sqlite.Build.CreateTable as B
import qualified Sqlite.Build.DropTable as B
import Data.Data (Proxy)
import Data.Proxy (Proxy(..))

class ResultCollection f where
  fromListResult :: [a] -> f a

instance ResultCollection [] where
  fromListResult = id

instance ResultCollection Maybe where
  fromListResult = listToMaybe

data Insert a r where
  Insert :: a -> Insert a ()
  InsertId :: a -> Insert a (ID a)
  InsertEntity :: Parsed a => a -> Insert a (Entity a)
  InsertMany :: [a] -> Insert a ()
  InsertMantId :: [a] -> Insert a [ID a]
  InsertManyEntity :: [a] -> Insert a [Entity a]
  InsertUnique :: HasUnique a => a -> Insert a Bool
  InsertUniqueId :: HasUnique a => a -> Insert a (Maybe (ID a))
  InsertUniqueEntity :: HasUnique a => a -> Insert a (Maybe (Entity a))

data Upsert a r where
  Upsert :: a -> Upsert a ()
  UpsertId :: a -> Upsert a (ID a)
  UpsertEntity :: Parsed a => a -> Upsert a (Entity a)

getUpsertEntity :: Upsert a r -> a
getUpsertEntity = \case 
  Upsert a -> a
  UpsertId a -> a
  UpsertEntity a -> a

data family Conn b

newtype instance Conn Sqlite = SqliteConn SQL.Connection

class Backend b where
  type IDCons b a :: Constraint
  createTable_ :: (IDDefinition (ID ty), IsTable ty) => Proxy ty -> Conn b -> IO ()
  dropTable_ :: IsTable ty => Proxy ty -> Conn b -> IO ()
  runQuery :: (Parsed a, ResultCollection f) => Conn b -> Query (f (RetRow a)) -> IO (f (Result a))
  runInsert :: (Parsed (Row a), IsTable a, IDCons b a) => Conn b -> Insert a r -> IO r
  runUpdate :: IsTable ty => Conn b -> Update ty () -> IO ()
  runUpsert :: (Parsed (Row a), IsTable a, IDCons b a) => Conn b -> Maybe (Unique a) -> Upsert a r -> [SetField a] -> IO r

createTable :: forall ty b. (IDDefinition (ID ty), IsTable ty, Backend b) => Conn b -> IO ()
createTable = createTable_ @b @ty Proxy

dropTable :: forall ty b. (IsTable ty, Backend b) => Conn b -> IO ()
dropTable = dropTable_ @b @ty Proxy

data Sqlite

instance Backend Sqlite where
  type
    IDCons Sqlite a =
      ( ID a ~ ID' a
      , IF
          (ID a == ID' a)
          ()
          (TE.TypeError ( 'TE.Text "Sqlite backend can't work with IDs different from " 'TE.:<>: 'TE.ShowType ID'))
      )

  -- createTable :: forall ty. IsTable ty => Conn Sqlite -> IO ()
  -- createTable_ :: forall ty. (IDDefinition (ID ty), IsTable ty) => Proxy ty -> Conn Sqlite -> IO ()
  createTable_ :: (IDDefinition (ID ty), IsTable ty) => Proxy ty -> Conn Sqlite -> IO ()
  createTable_ (_ :: Proxy ty) (SqliteConn conn)  = Sqlite.execute_ conn (Sqlite.Query $ toText $ B.createTable @ty)


  -- dropTable :: forall ty. _ => Conn Sqlite -> IO ()
  dropTable_ :: IsTable ty => Proxy ty -> Conn Sqlite -> IO ()
  dropTable_ (_ :: Proxy ty) (SqliteConn conn) = Sqlite.execute_ conn (Sqlite.Query $ toText $ B.dropTable @ty)


  runUpsert :: forall a r. (IsTable a, IDCons Sqlite a, Parsed (Row a)) => Conn Sqlite -> Maybe (Unique a) -> Upsert a r -> [SetField a] -> IO r
  runUpsert conn un i u = do
    let allUniques = universe @(Unique a)
    let checkUniques = maybe allUniques pure un
    let ty = getUpsertEntity i
    conflictEnt <- checkForUniques conn ty checkUniques
    case conflictEnt of
      Nothing -> runInsert @_ @a conn case i of
         Upsert a -> Insert a
         UpsertId a -> InsertId a
         UpsertEntity a -> InsertEntity a
      Just id' -> do
        runUpdate conn do
          L.update @a (\e -> do L.set u; where' (Q.eid @. e ==. Q.val id'))
        case i of 
          Upsert _ -> pure ()
          UpsertId _ -> pure id'
          UpsertEntity _ -> selectById conn id' >>= \case
             Nothing -> error "???"
             Just en -> pure en
        

  runUpdate ::  IsTable ty => Conn Sqlite -> Update ty () -> IO ()
  runUpdate (SqliteConn conn) u = do
      let (rawQ, Lite.BuildState params _) = runState (Lite.buildUpdate u) (Lite.BuildState [] 0)
      Sqlite.execute conn (Sqlite.Query $ toText rawQ) params

  runQuery :: forall a f. (Parsed a, ResultCollection f) => Conn Sqlite -> Query (f (RetRow a)) -> IO (f (Result a))
  runQuery (SqliteConn conn) q = do
    let (rawQ, Lite.BuildState params _) = runState (Lite.buildQuery q) (Lite.BuildState [] 0)
    res <- Sqlite.query conn (Sqlite.Query $ toText rawQ) params
    let parsed = forM res \r -> runEitherParser (map transformFrom r) (parseRow @a)
    case parsed of
      Left s -> error (s <> ": " <> show res)
      Right res' -> pure $ fromListResult res'

  runInsert :: forall a r. (IDCons Sqlite a, Parsed (Row a), IsTable a) => Conn Sqlite -> Insert a r -> IO r
  runInsert sConn@(SqliteConn conn) insertValue =

    case insertValue of
      Insert a -> insertRow a
      InsertId a -> insertId a
      InsertEntity a -> insertEntity a
      InsertMany as -> forM_ as insertRow
      InsertMantId as -> forM as insertId
      InsertManyEntity as -> forM as insertEntity
      InsertUnique a -> insertUnique_ a
      InsertUniqueId a -> insertUniqueId a
      InsertUniqueEntity a -> insertUniqueEntity a

    where
        cols = fold $ intersperse ", " $ map (\case SqlColumn n _ _-> coerce n) (Nel.tail $ columns @a)

        TableName tn = tableName @a
        phs = fold $ intersperse ", " $ map (const "?") $ Nel.tail (columns @a)

        insertRow a = do
          let vals = encode @a a <&> transformTo
          Sqlite.execute conn (Sqlite.Query $ toText $ insertPrefix tn <> cols <> insertSuffix phs) vals

        insertId a = do
          insertRow a
          ID' @a <$> Sqlite.lastInsertRowId conn

        insertEntity a = do
          insertRow a
          i <- ID' <$> Sqlite.lastInsertRowId conn
          e <- selectById @a (SqliteConn conn) i
          case e of
            Nothing -> error "Failed to load row"
            Just en -> pure en

        insertUniqueThen :: a -> (Maybe (ID' a) -> IO b) -> IO b
        insertUniqueThen a cont = do
          let vals = encode @a a <&> transformTo
          let allUniques = universe @(Unique a)
          conflictId <- checkForUniques sConn a allUniques
          case conflictId of
            Just _ -> cont Nothing 
            Nothing -> do
              Sqlite.execute conn (Sqlite.Query $ toText $ insertPrefix tn <> cols <> insertSuffix phs) vals
              lastID <- ID' @a <$> Sqlite.lastInsertRowId conn
              cont (Just lastID)

        insertUnique_ a = a `insertUniqueThen` do
          \case 
            Nothing -> pure False
            Just _ -> pure True

        insertUniqueId a = a `insertUniqueThen` do
          \case
            Nothing -> pure Nothing
            Just id' -> pure $ Just id'

        insertUniqueEntity a = a `insertUniqueThen` do
          \case 
            Nothing -> pure Nothing
            Just id' -> selectById @a (SqliteConn conn) id'



checkForUniques :: (TableColumns t, TableCodec t) => Conn Sqlite -> t -> [Unique t] -> IO (Maybe (ID t))
checkForUniques _ _ [] = pure Nothing
checkForUniques conn a (u: us) = do
  conflict <- getIdBy conn a (uniqueColumns u)
  case conflict of 
    Just v -> pure (Just v)
    Nothing -> checkForUniques conn a us 


toText :: BS.Builder -> TS.Text
toText = T.toStrict . T.decodeUtf8 . BS.toLazyByteString

selectById :: forall ty . IsTable ty => Conn Sqlite -> ID ty -> IO (Maybe (Entity ty))
selectById conn i = runQuery conn do
      as <- from @ty
      where' (eid @. as ==. val i)
      one as

insertPrefix :: BS.Builder -> BS.Builder
insertPrefix tn = "INSERT INTO " <> tn <> " ("

insertSuffix :: BS.Builder -> BS.Builder
insertSuffix phs = ") values (" <> phs <> ")"

getIdBy :: forall ty. (IsSqlValue (ID ty), IsTable ty) => Conn Sqlite -> ty -> Nel.NonEmpty (UniqueColumn ty) -> IO (Maybe (ID ty))
getIdBy conn ty cols =
  runQuery conn do
    a <- uniqueQuery @ty ty cols
    one (eid @. a)

uniqueQuery :: forall ty. (IsSqlValue (ID ty), IsTable ty) => ty -> Nel.NonEmpty (UniqueColumn ty) -> Query (Row ty)
uniqueQuery ty cols = do
  a <- from @ty
  where' (foldr1 (&&.) $ cols <&> \(UniqueColumn n f) -> TypedColumn n @. a ==. SqlExpr (Value $ f ty))
  pure a

getEntityBy :: forall ty. (IsSqlValue (ID ty), IsTable ty) => Conn Sqlite -> ty -> Nel.NonEmpty (UniqueColumn ty) -> IO (Maybe (Entity ty))
getEntityBy conn ty cols =
  runQuery conn do
    a <- uniqueQuery @ty ty cols
    one a
