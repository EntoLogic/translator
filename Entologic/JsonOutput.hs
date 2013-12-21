
{-# LANGUAGE OverloadedStrings #-}

module Entologic.JsonOutput where

import Entologic.Output
import Data.Aeson
import Control.Applicative
import Data.Maybe

instance ToJSON OutputNode where
    toJSON (OutputNode node clauses indent loc) =
        object [ "node" .= node, "clauses" .= clauses
               , "indent" .= indent, "location" .= loc]

instance ToJSON OutputClause where
    toJSON (OCString s) = toJSON s
    toJSON (OCNode n) = toJSON n
    toJSON (OCNodes ns) = toJSON ns
    toJSON (OCAnnot a) = toJSON a

instance ToJSON Annotation where
    toJSON _ = object []

instance ToJSON Area where
    toJSON (Area start end) =
        object . catMaybes $ [pair "start" start, pair "end" end]
      where
        pair name val = (name .=) . toJSON <$> val

instance ToJSON Location where
    toJSON (Location line col) = toJSON [line, col]
