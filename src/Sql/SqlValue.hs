{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Sql.SqlValue where

import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.String (IsString (..))
import qualified Data.Text as T
import Sql.SqlType (SqlType (..), TypeDown)

type Failure f r = [SqlValue] -> String -> f r
type Success a f r = [SqlValue] -> a -> f r

type Parser' a = forall f r. [SqlValue] -> Failure f r -> Success a f r -> f r

newtype Parser a = Parser {runParser :: Parser' a}

runEitherParser :: [SqlValue] -> Parser a -> Either String a
runEitherParser vals a = runParser a vals (const Left) (const Right)

instance Functor Parser where
  fmap g (Parser b) = Parser \vals f s -> b vals f (\vs a -> s vs (g a))

instance Applicative Parser where
  pure a = Parser \vals _ s -> s vals a
  a <*> b = do
    a' <- a
    a' <$> b

instance Monad Parser where
  (Parser m) >>= g = Parser \vals f s -> m vals f \vals' a -> runParser (g a) vals' f s

takeOne :: Parser SqlValue
takeOne = Parser \case
  [] -> \f _ -> f [] "Failed to parse query result. Unexpected EOF"
  (a : as) -> \_ s -> s as a

skipN :: Int -> Parser ()
skipN n = Parser \l _ s -> s (drop n l) ()

parseValue :: String -> (forall f r. (a -> f r) -> f r -> SqlValue -> f r) -> Parser a
parseValue tag h = Parser \vals f s -> case vals of
  [] -> f [] "Failed to parse query result. Unexpected EOF"
  sv : svs -> h (s svs) (f svs $ "Failed to parse " <> tag <> ". got: " <> typeOfValue sv) sv
  where
    typeOfValue a = case a of 
      SqlTextValue _ -> "TEXT"
      SqlBytesValue _ -> "BYTES"
      SqlIntValue _ -> "INT"
      SqlFloatValue _ -> "FLOAT"
      SqlBoolValue _ -> "BOOL"
      SqlNull -> "NULL"

newtype TypedSqlValue (t :: SqlType) = TypedSqlValue SqlValue

data SqlValue
  = SqlTextValue T.Text
  | SqlBytesValue B.ByteString
  | SqlIntValue Int64
  | SqlFloatValue Double
  | SqlBoolValue Bool
  | SqlNull

instance Show SqlValue where
  show = \case
    SqlTextValue txt -> show txt
    SqlBytesValue bs -> show bs
    SqlIntValue in' -> show in'
    SqlFloatValue x -> show x
    SqlBoolValue b -> show b
    SqlNull -> "null"

class TypeDown (SqlTypeOf a) => IsSqlValue a where
  type SqlTypeOf a :: SqlType
  toSqlValue :: a -> SqlValue
  fromSqlValue :: Parser a

instance IsSqlValue Int64 where
  type SqlTypeOf Int64 = 'SqlInt
  toSqlValue = SqlIntValue . fromIntegral
  fromSqlValue = parseValue "Int64" \s f -> \case
    SqlIntValue int -> s int
    _ -> f

instance IsSqlValue Int where
  type SqlTypeOf Int = 'SqlInt
  toSqlValue = SqlIntValue . fromIntegral
  fromSqlValue = parseValue "Int" \s f -> \case
    SqlIntValue int -> s $ fromIntegral int
    _ -> f

instance IsSqlValue String where
  type SqlTypeOf String = 'SqlText
  toSqlValue = SqlTextValue . fromString
  fromSqlValue = parseValue "String" \s f -> \case
    SqlTextValue txt -> s $ T.unpack txt
    _ -> f

instance IsSqlValue a => IsSqlValue (Maybe a) where
  type SqlTypeOf (Maybe a) = 'SqlNullable (SqlTypeOf a)
  toSqlValue = \case
    Nothing -> SqlNull
    Just a -> toSqlValue a

  fromSqlValue = Parser \vals f s -> case vals of
    [] -> f [] "Failed to parse query result. Unexpected EOF"
    (SqlNull : rest) -> s rest Nothing
    _ -> runParser (fmap Just fromSqlValue) vals f s
