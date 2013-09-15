
{-# LANGUAGE OverloadedStrings #-}

module Entologic.JSONDecode where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Applicative((<$>), (<*>), pure)
import Data.Text

import Entologic.AST


instance FromJSON Function where
    parseJSON (Object obj) = do
       (String "Function") <- obj .: "Node"
       Function <$> obj .: "Name"
                <*> obj .:? "RetType"
                <*> obj .: "Arguments"
                <*> obj .: "Body"
--                <*> obj .:? "Extra"
                <*> pure Nothing

instance FromJSON Type where
    parseJSON (String s) = return $ StringT s

instance FromJSON ParamDecl where
    parseJSON (String s) = return $ ParamDecl s Nothing
    parseJSON (Object obj) = ParamDecl <$> obj .: "Name"
                                       <*> obj .:? "Type"

instance FromJSON Body where
    parseJSON (Array stms) = Body <$> mapM parseJSON (V.toList stms)

instance FromJSON Statement where
    parseJSON (Object obj) = do
        (String typ) <- obj .: "Node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> fail "Invalid Statement type"
      where
        parsers = [("VarDecl", varDecl)]

varDecl :: Object -> Parser Statement
varDecl obj = VarDecl <$> obj .: "Type"
                      <*> obj .: "Name"
                      <*> obj .:? "Value"

instance FromJSON Expression where
    parseJSON (Object obj) = do
        (String typ) <- obj .: "Node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> fail "Invalid Expression type"
      where
        parsers = [] :: [(Text, Object -> Parser Expression)]
