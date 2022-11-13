{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Sql.Table.Columns.TH where

import Control.Monad (forM, forM_, when)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty (fromList)
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField(..))
import Language.Haskell.TH
  ( Body (..)
  , Con (RecC)
  , Dec (..)
  , Exp (..)
  , Info (..)
  , Lit (..)
  , Name
  , Pat (..)
  , Q
  , TyLit (..)
  , TySynEqn (..)
  , Type (..)
  , mkName
  )
import Language.Haskell.TH.Syntax (Name (..), OccName (..), reify)
import Sql.SqlType (TypeDown (..))
import Sql.SqlValue (IsSqlValue (..))
import Sql.Table.Columns
import Sql.Table.TableInfo (ID (..), fullTableName, idColumnName)
import Sql.Types (ColumnName (ColumnName), IDReference (..), SqlColumn (..), TypedColumn (..), UniqueColumn (..))

data Replace = Replace {from :: String, to :: String}
  deriving (Show)

makeGenericColumns :: Name -> [Replace] -> Q [Dec]
makeGenericColumns dataName replace = do
  selectorsWithTypes <- check dataName replace

  let namesWithSqlNamesWithTypes = getNamesWithSqlNamesWithTypes replace selectorsWithTypes

  let wholeType = ConT dataName

  cols <- concat <$> forM namesWithSqlNamesWithTypes \(name, sqlName, ty) -> do
    forType <- case ty of
      AppT (ConT t) idOf | t == ''ID -> do
        FamilyI _ idInstances <- reify t
        let mbIdInstance = flip mapMaybe idInstances \case
              TySynInstD (TySynEqn _ (AppT _ a) x) | a == idOf -> Just x
              _ -> Nothing
        case mbIdInstance of
          [x] -> pure x
          _ -> fail "Failed to find instance for ID"
      _ -> pure ty

    let nameType = LitT (StrTyLit name)
    let typeVar = VarT (mkName "a")
    let ctx = [ConT ''HasField .$ nameType .$ wholeType .$ forType, EqualityT .$ forType .$ typeVar]
    let rowInstanceHead = ConT ''IsLabel .$ nameType .$ (ConT ''TypedColumn .$ wholeType .$ typeVar)
    let uniqueInstanceHead = ConT ''IsLabel .$ nameType .$ (ConT ''UniqueColumn .$ wholeType)
    sqlNameE <- [|sqlName|]
    let rowBody =
          [ SigD 'fromLabel (ConT ''TypedColumn .$ wholeType .$ typeVar)
          , ValD (VarP 'fromLabel) (NormalB (ConE 'TypedColumn .& (ConE 'ColumnName .& sqlNameE))) []
          ]

    let uniqueBody =
          [ SigD 'fromLabel (ConT ''UniqueColumn .$ wholeType)
          , ValD 
              (VarP 'fromLabel) 
              (NormalB $
                ConE 'UniqueColumn .& (ConE 'ColumnName .& sqlNameE) .& (VarE '(.) .& VarE 'toSqlValue .& AppTypeE (VarE 'getField) nameType)
              ) []
          ]

    let rowInstance = InstanceD Nothing ctx rowInstanceHead rowBody
    let uniqueInstance = InstanceD Nothing ctx uniqueInstanceHead uniqueBody
    pure [rowInstance, uniqueInstance]


  colClass <- deriveColumnClass dataName $ map (\(_, a, b) -> (a, b)) namesWithSqlNamesWithTypes
  pure (colClass : cols)


check :: Name -> [Replace] -> Q [(String, Type)]
check dataName replace = do
  definition <- reify dataName
  selectors <- case definition of
    TyConI (DataD _ _ _ _ [RecC _ selectors] _) -> pure selectors
    _ -> fail "The first argument must be data which has exactly one constructor with records"
  let selectorsWithTypes = map (\(Name (OccName name) _, _, ty) -> (name, ty)) selectors
  let selectorNames = map fst selectorsWithTypes
  forM_ replace \(Replace f _) ->
    when (f `notElem` selectorNames) do
      fail $ "Selector `" <> f <> "` is not presented in " <> show dataName
  pure selectorsWithTypes

type MakeSelectorName = String -> String

makeColumns :: Name -> [Replace] -> MakeSelectorName -> [String] -> Q [Dec]
makeColumns dataName replace makeSelectorName _ = do
  selectorsWithTypes <- check dataName replace

  let wholeType = ConT dataName

  let namesWithSqlNamesWithTypes = getNamesWithSqlNamesWithTypes replace selectorsWithTypes

  cols <-
    concat <$> forM namesWithSqlNamesWithTypes \(name, sqlName, forType) -> do
      sqlNameE <- [|sqlName|]

      let body f =
            let selectorName = mkName $ f name
             in [ SigD selectorName (ConT ''TypedColumn .$ wholeType .$ forType)
                , ValD (VarP 'fromLabel) (NormalB (ConE 'TypedColumn .& (ConE 'ColumnName .& sqlNameE))) []
                ]
      pure (body makeSelectorName)

  colClass <- deriveColumnClass dataName $ map (\(_, a, b) -> (a, b)) namesWithSqlNamesWithTypes
  pure (colClass : cols)

getNamesWithSqlNamesWithTypes :: [Replace] -> [(String, Type)] -> [(String, String, Type)]
getNamesWithSqlNamesWithTypes replace selectorsWithTypes =
  selectorsWithTypes <&> \(name, forType) ->
    let sqlName = maybe name to (find (\(Replace f _) -> f == name) replace)
     in (name, sqlName, forType)

deriveColumnClass :: Name -> [(String, Type)] -> Q Dec
deriveColumnClass dataName def = do
  let idCol =
        ConE 'SqlColumn
          .& AppTypeE (VarE 'idColumnName) (ConT dataName)
          .& AppTypeE (VarE 'down) (ConT ''SqlTypeOf .$ (ConT ''ID .$ ConT dataName))
          .& ConE 'Nothing
  let cols =
        def <&> \(colName, ty) ->
          let reference = case ty of
                AppT (ConT t) idOf
                  | t == ''ID ->
                      ConE 'Just
                        .& ( ConE 'IDReference
                              .& AppTypeE (VarE 'fullTableName) idOf
                              .& AppTypeE (VarE 'idColumnName) idOf
                           )
                _ -> ConE 'Nothing
           in ConE 'SqlColumn
                .& (VarE 'fromString .& LitE (StringL colName))
                .& AppTypeE (VarE 'down) (ConT ''SqlTypeOf .$ ty)
                .& reference
  pure $
    InstanceD
      Nothing
      []
      (ConT ''TableColumns .$ ConT dataName)
      [ValD (VarP 'columns) (NormalB $ VarE 'fromList .& ListE (idCol : cols)) []]

second3 :: (a, b, c) -> b
second3 (_, b, _) = b

(.$) :: Type -> Type -> Type
(.$) = AppT

infixl 1 .$

(.&) :: Exp -> Exp -> Exp
(.&) = AppE

infixl 1 .&

(~>) :: String -> String -> Replace
(~>) = Replace
