{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sql.SqlType where

data SqlType
  = SqlText
  | SqlBytes
  | SqlInt
  | SqlFloat
  | SqlBool
  | SqlAny
  | SqlNullable SqlType
  deriving (Show)

class TypeDown (t :: SqlType) where
  down :: SqlType

instance TypeDown 'SqlText where down = SqlText
instance TypeDown 'SqlBytes where down = SqlBytes
instance TypeDown 'SqlInt where down = SqlInt
instance TypeDown 'SqlFloat where down = SqlFloat
instance TypeDown 'SqlBool where down = SqlBool
instance TypeDown 'SqlAny where down = SqlAny
instance TypeDown a => TypeDown ( 'SqlNullable a) where down = SqlNullable (down @a)
