
{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             GeneralizedNewtypeDeriving,
             TemplateHaskell,
             MultiParamTypeClasses,
             InstanceSigs,
             TypeSynonymInstances,
             FlexibleInstances,
             OverloadedStrings #-}

module Entologic.Ast where

import Data.Text
import Control.Monad.IO.Class

import Entologic.Base
import Entologic.Output

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Trans.State.Lazy(StateT(..), evalStateT)
import Control.Monad.State.Class
import Control.Monad.Trans.Reader(ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Error(ErrorT(..))
import Control.Monad.Trans.Class
import Control.Lens.TH


data UAst = UAst {uMeta :: AstMeta, uProg :: Program} deriving Show

data AstMeta = AstMeta { mPLang :: PLang, mSLang :: SLang }
               deriving (Show)

class AstNode a where
    translate :: AN a -> TL OutputClause
    translate = undefined
    name :: a -> Text
    name = const ""

type AN a = (a, Area)

type Text' = AN Text
type String' = AN String

type ProgramEntry' = AN ProgramEntry
data ProgramEntry = PEFunc Function
                  | PECls Class
                  | PEStm Statement
                  deriving (Show)

data Program = Program { pEntries :: [ProgramEntry'] }
               deriving (Show)

type Type' = AN Type
data Type = StringT Text
--          | forall a. (ASTNode a, Show a) => LSType a
            deriving (Show, Ord, Eq)


type LSAny' = AN LSAny
data LSAny = forall a. (AstNode a, Show a) => LSAny a

instance Show LSAny where
    show _ = "LSAny"

type Class' = AN Class
data Class = Class { cName :: String'
                   , cSuperCls :: Maybe Type'
                   , cMembers :: [Member]
                   }
             deriving (Show)

type Member' = AN Member
data Member = MFunc Function'
            | MField Field'
              deriving (Show)

type Field' = AN Field
data Field = Field
             deriving (Show)

type Function' = AN Function
data Function = Function { fName :: String'
                         , fRTyp :: Maybe Type'
                         , fParams :: [ParamDecl']
                         , fBody :: Body'
                         , fExtra :: Maybe LSAny' 
                         }
                deriving (Show)

type ParamDecl' = AN ParamDecl
data ParamDecl = ParamDecl { pName :: Text', pType :: Maybe Type' }
                 deriving (Show, Ord, Eq)

type Body' = AN Body
data Body = Body [Statement']
            deriving (Show, Ord, Eq)

type Statement' = AN Statement
data Statement = VarDecl { vdType :: Type'
                         , vdName :: Text'
                         , vdInit :: Maybe Expression'
                         }
               | StmExpr Expression
                 deriving (Show, Ord, Eq)

type VarRef' = AN VarRef
data VarRef = StringV Text
            | DottedV [Text]
              deriving (Show, Ord, Eq)

type InfixOp' = AN InfixOp
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
             | Gt
             | Lt
             | GtEq
             | LtEq
             | Equal
             | NEqual
               deriving (Show, Ord, Eq)

type PrefixOp' = AN PrefixOp
data PrefixOp = Neg
              | BInv
              | PreInc
              | PreDec
                deriving (Show, Ord, Eq)

type PostfixOp' = AN PostfixOp
data PostfixOp = PostInc
               | PostDec
                 deriving (Show, Ord, Eq)

type Expression' = AN Expression
data Expression = Assign VarRef' Expression'
                | OpAssign VarRef' InfixOp' Expression'
                | BinOp InfixOp' Expression' Expression'
                | IntLit Integer
                | StringLit Text
                | PreOp PrefixOp' Expression'
                | PostOp PostfixOp' Expression'
                  deriving (Show, Ord, Eq)
