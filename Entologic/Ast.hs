
{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             GeneralizedNewtypeDeriving,
             TemplateHaskell,
             MultiParamTypeClasses,
             InstanceSigs #-}

module Entologic.Ast where

import Data.Text
import Database.MongoDB.Query
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Entologic.Phrase

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Trans.State.Lazy(StateT(..))
import Control.Monad.State.Class
import Control.Monad.Trans.Reader(ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Lens.TH

data TLState = TLState
data TLInfo = TLInfo { _tlPhrases :: Phrases
                     , _tlPLang :: PLang
                     , _tlSLang :: SLang
                     }
              deriving (Eq, Ord, Show)

$(makeLenses ''TLInfo)

type WebTranslator = IO

newtype TL a = TL (ReaderT TLInfo (StateT TLState WebTranslator) a)
               deriving (Functor, Applicative, Monad)

instance MonadState TLState TL where
    get = TL get
    put = TL . put

instance MonadReader TLInfo TL where
    ask = TL ask
    local f (TL m) = TL $ local f m

class AstNode a where
    translate :: a -> TL Text
    translate = undefined

data NInf = NInf {lineNo :: Int}
            deriving (Show)

data ProgramEntry = PEFunc Function
                  | PECls Class
                  | PEStm Statement
                  deriving (Show)

data Program = Program { pEntries :: [ProgramEntry] }
               deriving (Show)

data Type = StringT Text
--          | forall a. (ASTNode a, Show a) => LSType a
            deriving (Show)


data LSAny = forall a. (AstNode a, Show a) => LSAny a

instance Show LSAny where
    show _ = "LSAny"

data Class = Class { cName :: String
                   , cSuperCls :: Maybe Type
                   , cMembers :: [Member]
                   }
             deriving (Show)

data Member = MFunc Function
            | MField Field
              deriving (Show)

data Field = Field
             deriving (Show)

data Function = Function { fName :: String
                         , fRTyp :: (Maybe Type)
                         , fParams :: [ParamDecl]
                         , fBody :: Body
                         , fExtra :: (Maybe LSAny) 
                         }
                deriving (Show)

data ParamDecl = ParamDecl { pName :: Text, pType :: (Maybe Type) }
                 deriving (Show)

data Body = Body [Statement]
            deriving (Show)

data Statement = VarDecl { vdType :: Type
                         , vdName :: Text
                         , vdInit :: Maybe Expression
                         }
               | StmExpr Expression
                 deriving (Show)

data VarRef = StringV Text
            | DottedV [Text]
              deriving (Show)

data InfixOp = Plus
             | Minus
             | Mult
             | Div
             | Mod
             | LOr
             | LAnd
             | BOr
             | BAnd
             | Xor
             | RShift
             | LShift
             | RUShift
               deriving (Show)

data PrefixOp = Neg
              | BInv
              | PreInc
              | PreDec
                deriving (Show)

data PostfixOp = PostInc
               | PostDec
                 deriving (Show)

data Expression = Assign VarRef Expression
                | OpAssign VarRef InfixOp Expression
                | BinOp InfixOp Expression Expression
                | IntLit Integer
                | StringLit Text
                | PreOp PrefixOp Expression
                | PostOp PostfixOp Expression
                  deriving (Show)
