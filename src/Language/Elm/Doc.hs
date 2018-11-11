{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Elm.Doc
    ( Docs
    , ModuleDoc
        ( moduleDocName
        , moduleDocComment
        , moduleDocValues
        )
    , ValueDoc
        ( valueDocName
        , valueDocComment
        , valueDocType
        )
    )
where

import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)

import Data.Aeson ((.:))
import Data.Text (Text)


type Docs = [ModuleDoc]


data ModuleDoc = ModuleDoc
    { moduleDocName    :: Text
    , moduleDocComment :: Text
    , moduleDocValues  :: [ValueDoc]
    }


instance Aeson.FromJSON ModuleDoc where
    parseJSON :: Aeson.Value -> Aeson.Parser ModuleDoc
    parseJSON = Aeson.withObject "Module" $ \object -> do
        moduleDocName    <- object .: "name"
        moduleDocComment <- object .: "comment"
        moduleDocValues  <- object .: "values"
        pure ModuleDoc {..}


data ValueDoc = ValueDoc
    { valueDocName    :: Text
    , valueDocComment :: Text
    , valueDocType    :: Text
    }


instance Aeson.FromJSON ValueDoc where
    parseJSON :: Aeson.Value -> Aeson.Parser ValueDoc
    parseJSON = Aeson.withObject "Value" $ \object -> do
        valueDocName    <- object .: "name"
        valueDocComment <- object .: "comment"
        valueDocType    <- object .: "type"
        pure ValueDoc {..}
