
{-# LANGUAGE TemplateHaskell #-}

module Entologic.Output
    ( OutputNode(..)
    , OutputClause(..)
    , Annotation(..)
    , Area(..)
    , Location(..)
    , oNode
    , oClauses
    , oIndent
    , loc
    ) where

import Data.Text

import Control.Lens.TH

import qualified Database.MongoDB as DB

import Entologic.Base

data OutputNode = OutputNode { _oNode :: Text
                             , _oClauses :: [OutputClause]
                             , _oIndent :: Bool
                             , _loc :: Area
                             , _source :: Text }
                  deriving (Eq, Ord, Show)

data OutputClause = OCString Text
                  | OCNode { ocNode :: OutputNode }
                  | OCNodes [OutputNode]
                  | OCAnnot Annotation
                    deriving (Eq, Ord, Show)

data Annotation = NoAnnotation
                  deriving (Eq, Ord, Show)

$(makeLenses ''OutputNode)
$(makeLenses ''Area)
$(makeLenses ''Location)
