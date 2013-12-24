
{-# LANGUAGE TemplateHaskell #-}

module Entologic.Output where

import Data.Text
import Control.Lens.TH

import Entologic.Base

data OutputNode = OutputNode { _oNode :: Text
                             , _oClauses :: [OutputClause]
                             , _oIndent :: Bool
                             , _loc :: Area }
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
