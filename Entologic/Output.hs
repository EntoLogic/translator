
{-# LANGUAGE TemplateHaskell #-}

module Entologic.Output where

import Data.Text
import Control.Lens.TH

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

data Area = Area { _start :: Maybe Location, _end :: Maybe Location }
            deriving (Eq, Ord, Show)

data Location = Location { _line :: Int, _col :: Int }
                deriving (Eq, Ord, Show)

$(makeLenses ''OutputNode)
$(makeLenses ''Area)
$(makeLenses ''Location)
