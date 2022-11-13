{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Kind (Constraint)

type family IF c t f :: Constraint where
  IF 'True t f = t
  IF 'False t f = f
