{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Backend where
import qualified Database.SQLite.Simple as Sqlite
import Control.Monad.State.Strict (runState)
import Database.SQLite.Simple.FromField ()
import Control.Monad ( forM, forM_ )
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy as T
import Sql.Query (Parsed (..), from, where', one, eid, (==.), val, (@.))
import Sql.Query.Types (Query)
import Sql.Types (RetRow, TableName (..), ColumnName (..), SqlColumn (..))
import Sql.SqlValue (SqlValue(..), runEitherParser)
import Sql.Table (IsTable (..), ID'(..), Entity)
import Data.List (intercalate, intersperse)
import Data.String (IsString(..))
import Data.Functor ((<&>))
import Data.Kind (Constraint)
import qualified GHC.TypeLits as TE
import Data.Type.Equality (type (==))
import Sql.Update (Update, SetField)
import Types (IF)
import qualified Database.SQLite.Simple as SQL
import Sql.Table.TableInfo (HasUnique, TableInfo(..))
import Sql.Table.Columns (TableColumns(..))
import Sql.Table.TableCodec (TableCodec(..))
import Data.Coerce (coerce)
import qualified Sqlite.Build.Types as Lite
import qualified Sqlite.Build.Update as Lite
import qualified Sqlite.Build.Query as Lite

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

data family Conn b

newtype instance Conn Sqlite = SqliteConn SQL.Connection

class Backend b where
  type IDCons b a :: Constraint
  query :: (Parsed a, ResultCollection f) => Conn b -> Query (f (RetRow a)) -> IO (f (Result a))
  insert :: (Parsed a, IsTable a, IDCons b a) => Conn b -> Insert a r -> IO r
  update :: Conn b -> Update ty () -> IO ()
  upsert :: IsTable a => Conn b -> a -> [SetField a] -> IO ()

data Sqlite

-- instance Backend Sqlite where

--   type IDCons Sqlite a =
--       ( ID a ~ ID' a,
--        IF (ID a == ID' a)
--         ()
--         (TE.TypeError ('TE.Text "Sqlite backend can't work with IDs different from " 'TE.:<>: 'TE.ShowType ID'))
--        )

--   -- upsert conn i u = do



--   update (SqliteConn conn) u = do
--     let (rawQ, Lite.BuildState params _) = runState (Lite.buildUpdate u) (Lite.BuildState [] 0)
--     Sqlite.execute conn (Sqlite.Query $ T.toStrict $ T.toLazyText rawQ) params

--   query :: forall a f. (Parsed a, ResultCollection f) => Conn Sqlite -> Query (f (RetRow a)) -> IO (f (Result a))
--   query (SqliteConn conn) q = do
--     let (rawQ, Lite.BuildState params _) = runState (Lite.buildQuery q) (Lite.BuildState [] 0)
--     res <- Sqlite.query @_ @[Sqlite.SQLData] conn (Sqlite.Query $ T.toStrict $ T.toLazyText rawQ) params
--     let parsed = forM res \r -> runEitherParser (map transformFrom r) (parseRow @a)
--     case parsed of
--       Left s -> error (s <> ": " <> show res)
--       Right res' -> pure $ fromListResult res'
  
--   insert :: forall a r. (IDCons Sqlite a, Parsed a, IsTable a) => Conn Sqlite -> Insert a r -> IO r
--   insert (SqliteConn conn) insertValue = do

--     case insertValue of
--       Insert a -> insertRow a
--       InsertId a -> insertId a
--       InsertEntity a -> insertEntity a
--       InsertMany as -> forM_ as insertRow
--       InsertMantId as -> forM as insertId
--       InsertManyEntity as -> forM as insertEntity
--       InsertUnique a -> insertUnique_ a
--       InsertUniqueId a -> insertUniqueId a
--       InsertUniqueEntity a -> insertUniqueEntity a

--     where
--         cols = intersperse ", " $ map (\case SqlColumn n _ _-> coerce n) $ columns @a

--         TableName tn = tableName @a
--         phs = intersperse ", " $ map (const "?") $ columns @a

--         insertRow a = do
--           let vals = encode @a a <&> transformTo 
--           Sqlite.execute conn (insertPrefix tn <> cols <> insertSuffix phs) vals

--         insertId a = do
--           insertRow a
--           ID' @a <$> Sqlite.lastInsertRowId conn

--         insertEntity a = do
--           insertRow a
--           i <- ID' <$> Sqlite.lastInsertRowId conn
--           e <- selectById @a (SqliteConn conn) i
--           case e of
--             Nothing -> error "Failed to load row"
--             Just en -> pure en
          
--         insertUniqueThen :: a -> (ID' a -> ID' a -> IO b) -> IO b
--         insertUniqueThen a cont = do
--           lastID <- ID' @a <$> Sqlite.lastInsertRowId conn
--           let vals = encode @a a <&> transformTo 
--           Sqlite.execute conn (fromString $ insertPrefix tn <> cols <> insertSuffix phs <> onConflict) vals
--           lastID' <- ID' @a <$> Sqlite.lastInsertRowId conn
--           cont lastID lastID'

--         insertUnique_ a = a `insertUniqueThen` do 
--           \i i' -> pure (i /= i')

--         insertUniqueId a = a `insertUniqueThen` do 
--           \i i' -> pure if i /= i' then Just i' else Nothing

--         insertUniqueEntity a = a `insertUniqueThen` do 
--           \i i' -> if i == i' 
--             then pure Nothing 
--             else selectById @a (SqliteConn conn) i'

--         onConflict = ""


-- selectById :: forall ty . IsTable ty => Conn Sqlite -> ID ty -> IO (Maybe (Entity ty))
-- selectById conn i = query conn do
--       as <- from @ty
--       where' (eid @. as ==. val i)
--       one as

-- insertPrefix :: [Char] -> [Char]
-- insertPrefix tn = "INSERT INTO " <> tn <> " ("

-- insertSuffix :: [Char] -> [Char]
-- insertSuffix phs = ") values (" <> phs <> ")"

-- transformFrom :: Sqlite.SQLData -> SqlValue
-- transformFrom (a :: Sqlite.SQLData) = case a of
--       Sqlite.SQLInteger in' -> SqlIntValue in'
--       Sqlite.SQLFloat x -> SqlFloatValue x
--       Sqlite.SQLText txt -> SqlTextValue txt
--       Sqlite.SQLBlob bs -> SqlBytesValue bs
--       Sqlite.SQLNull -> SqlNull

