
{-# LANGUAGE TemplateHaskell #-}

module Entologic.Output where

import Data.Text
import Control.Lens.TH

data OutputNode = OutputNode { _oNode :: Text
                             , _oClauses :: [OutputClause]
                             , _oIndent :: Bool
                             , _loc :: Area }

data OutputClause = OCString Text
                  | OCNode OutputNode
                  | OCAnnot Annotation

data Annotation = NoAnnotation

data Area = Area { _start :: Maybe Location, _end :: Maybe Location }

data Location = Location { _line :: Int, _col :: Int }

$(makeLenses ''OutputNode)
$(makeLenses ''Area)
$(makeLenses ''Location)
