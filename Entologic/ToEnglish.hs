
module Entologic.ToEnglish where

import Data.Map as M

type DB = M.Map String String

instance ASTNode Program where
    toEng db node =
