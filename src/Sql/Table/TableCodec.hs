{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sql.Table.TableCodec where

import qualified GHC.Generics as G
import Sql.SqlValue (IsSqlValue (..), Parser, SqlValue)

class TableCodec ty where
  decode :: Parser ty
  default decode :: (GenericDecode (G.Rep ty), G.Generic ty) => Parser ty
  decode = G.to <$> genericDecode

  encode :: ty -> [SqlValue]
  default encode :: (GenericEncode (G.Rep ty), G.Generic ty) => ty -> [SqlValue]
  encode c = genericEncode (G.from c)

class GenericEncode (a :: k -> *) where
  genericEncode :: a x -> [SqlValue]

instance (GenericEncode b) => GenericEncode (G.D1 a b) where
  genericEncode (G.M1 c) = genericEncode @_ @b c

instance (GenericEncode b) => GenericEncode (G.C1 a b) where
  genericEncode (G.M1 c) = genericEncode @_ @b c

instance (GenericEncode b, GenericEncode a) => GenericEncode (a G.:*: b) where
  genericEncode (a G.:*: b) =
    let a' = genericEncode a
        b' = genericEncode b
     in a' <> b'

instance (IsSqlValue ty) => GenericEncode (G.S1 ('G.MetaSel ('Just name) dc1 dc2 dc3) (G.Rec0 ty)) where
  genericEncode (G.M1 (G.K1 a)) = [toSqlValue a]

class GenericDecode (a :: k -> *) where
  genericDecode :: Parser (a x)

instance (GenericDecode b) => GenericDecode (G.D1 a b) where
  genericDecode = G.M1 <$> genericDecode @_ @b

instance (GenericDecode b) => GenericDecode (G.C1 a b) where
  genericDecode = G.M1 <$> genericDecode @_ @b

instance
  (GenericDecode b, GenericDecode a)
  => GenericDecode (a G.:*: b)
  where
  genericDecode = do
    a' <- genericDecode
    b' <- genericDecode
    pure (a' G.:*: b')

instance (IsSqlValue ty) => GenericDecode (G.S1 ( 'G.MetaSel ( 'Just name) dc1 dc2 dc3) (G.Rec0 ty)) where
  genericDecode = G.M1 . G.K1 <$> genericDecodeVal @ty

genericDecodeVal :: IsSqlValue ty => Parser ty
genericDecodeVal = fromSqlValue
