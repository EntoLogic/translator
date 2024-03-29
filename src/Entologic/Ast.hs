
{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           , OverloadedStrings
           , CPP
           #-}

module Entologic.Ast
    ( UAst(..)
    , AstMeta(..)
    , AstNode(..)
    , UK(..)
    , AN
    , toAn
    , Text'
    , String'
    , Program(..)
    , ProgramEntry(..)
    , Import(..)
    , Import'
    , Modifier(..)
    , Modifier'
    , Modifiers
    , modifiers
    , Type(..)
    , Type'
    , PrimType(..)
    , TypeDeclaration(..)
    , TypeDeclaration'
    , Class(..)
    , Class'
    , ClassTypePart(..)
    , ClassTypePart'
    , InClassDecl(..)
    , InClassDecl'
    , Member(..)
    , Member'
    , Field(..)
    , Field'
    , Function(..)
    , Function'
    , ParamDecl(..)
    , ParamDecl'
    , ParamDeclExtra(..)
    , Body(..)
    , Body'
    , Block
    , Statement(..)
    , Statement'
    , Case(..)
    , Case'
    , ForInit(..)
    , ForInit'
    , OneVarDecl(..)
    , OneVarDecl'
    , VarRef(..)
    , VarRef'
    , InfixOp(..)
    , InfixOp'
    , infixOps
    , PrefixOp(..)
    , PrefixOp'
    , PostfixOp(..)
    , PostfixOp'
    , TernaryOp(..)
    , TernaryOp'
    , Expression(..)
    , Expression'
    , GenericParam(..)
    , GenericParam'
    , GenericParamDecl(..)
    , GenericParamDecl'
    , Area(..)
    , Location(..)
    , errNode
    ) where


import Data.Text
import Data.String (IsString(..))
import qualified Data.Map as M

import Control.Applicative hiding (Const(..))
import Control.Monad.Trans.State.Lazy (StateT(..), evalStateT)
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Lens.TH

import Entologic.Base
import Entologic.Output


data UAst = UAst {uMeta :: AstMeta, uProg :: Program} deriving Show

data AstMeta = AstMeta --{ mPLang :: PLang, mSLang :: SLang }
               deriving (Show)

errNode :: Text -> OutputNode
errNode e = OutputNode "Unknown" [OCString e] False (Area Nothing Nothing) ""

class AstNode a where
    translate :: AN a -> TL OutputNode
    translate (ukn, area) =
      case ukn of
        Node n -> translate' (n, area)
        Unknown err -> return $ errNode err
    translate' :: (a, Area) -> TL OutputNode

    name :: a -> Text
    name = const ""

-- A node that might be unknown, in which case an error is given
data UK a = Node a
          | Unknown Text
            deriving (Eq, Ord, Show)

type AN a = (UK a, Area)

#define AN_(a) type a' = AN a

toAn :: a -> AN a
toAn a = (Node a, Area Nothing Nothing)

type Text' = AN Text
type String' = AN String

type ProgramEntry' = AN ProgramEntry
data ProgramEntry = PEFunc Function
                  | PECls Class
                  | PEStm Statement
                  deriving (Show)

data Program = Program { pEntries :: [ProgramEntry'] }
             | CompilationUnit { cuPkg :: Maybe [Text']
                               , cuImports :: [Import']
                               , cuTypeDecls :: [TypeDeclaration']
                               }
               deriving (Show)

type Import' = AN Import
data Import = Import { imports :: [Text] }
            | ImportAll { imports :: [Text] }
            | ImportStatic { imports :: [Text] }
            | ImportStaticAll { imports :: [Text] }
              deriving (Show, Ord, Eq)

type Modifiers = [Modifier']
type Modifier' = AN Modifier
data Modifier = Public
              | Protected
              | Private
              | Static
              | Const
              | Final
              | Abstract
              | Transient
              | Volatile
              | Synchronized
              | StrictFP
              | Native
                deriving (Show, Ord, Eq)

modifiers :: [(Text, Modifier)]
modifiers =
    [("public", Public), ("private", Private) , ("protected", Protected)
    , ("static", Static), ("const", Const), ("final", Final)
    , ("abstract", Abstract), ("transient", Transient), ("volatile", Volatile)
    , ("synchronized", Synchronized), ("strictfp", StrictFP)
    , ("native", Native)]

data PrimType = IntT
              | LongT
              | ShortT
              | ByteT
              | CharT
              | FloatT
              | DoubleT
              | LDoubleT
              | BooleanT
                deriving (Show, Ord, Eq)

type Type' = AN Type
            -- ClassType: a list of (dot-separated) parts, some of which can be
            -- types with generic parameters
data Type = ClassType [ClassTypePart']
          | StringT Text
          | ArrayType Type'
          | PrimType PrimType
--          | forall a. (ASTNode a, Show a) => LSType a
            deriving (Show, Ord, Eq)

type ClassTypePart' = AN ClassTypePart
data ClassTypePart = ClassTypePart Text' [GenericParam']
                     deriving (Eq, Ord, Show)


type LSAny' = AN LSAny
data LSAny = forall a. (AstNode a, Show a) => LSAny a

instance Show LSAny where
    show _ = "LSAny"

type TypeDeclaration' = AN TypeDeclaration
data TypeDeclaration = TDCls Class
                     | TDEnum EnumDecl
                     | TDInterface Interface
                     | TDAnnotation AnnotationDecl
                       deriving (Show, Ord, Eq)

data Interface = Interface
                 deriving (Show, Ord, Eq)
data EnumDecl = EnumDecl
                deriving (Show, Ord, Eq)
data AnnotationDecl = AnnotationDecl
                      deriving (Show, Ord, Eq)

type Class' = AN Class
data Class = Class { cMods :: Modifiers
                   , cName :: Text'
                   , cGericParams :: [GenericParamDecl']
                   , cSuperCls :: Maybe Type'
                   , cInterfaces :: [Type']
                   , cMembers :: [InClassDecl']
                   }
             deriving (Show, Ord, Eq)

type InClassDecl' = AN InClassDecl
data InClassDecl = MemberDecl Member
                 | InitBlock Block
                 | StaticInitBlock Block
                   deriving (Show, Ord, Eq)

type Member' = AN Member
data Member = MFunc Function
            | MField Field
              deriving (Show, Ord, Eq)

type Field' = AN Field
data Field = Field
             deriving (Show, Ord, Eq)

type Function' = AN Function
data Function = Function { fMods :: Modifiers
                         , fRTyp :: Maybe Type'
                         , fName :: Text'
                         , fParams :: [ParamDecl']
                         , fBody :: Maybe Block
                         }
                deriving (Show, Ord, Eq)

type ParamDecl' = AN ParamDecl
data ParamDecl = ParamDecl { pName :: Text'
                           , pType :: Maybe Type'
                           , pModifiers :: Modifiers
                           , pDefault :: Maybe Expression'
                           , pExtra :: Maybe ParamDeclExtra
                           }
                 deriving (Show, Ord, Eq)

data ParamDeclExtra = VarArgs
                    | MapArgs
                      deriving (Show, Ord, Eq)

type Body' = AN Body
data Body = Body [Statement']
            deriving (Show, Ord, Eq)

type Block = [Statement']

type Statement' = AN Statement
data Statement = VarDecl { vdMods :: Modifiers
                         , vdType :: Maybe Type'
                         , vdName :: Text'
                         , vdInit :: Maybe Expression'
                         }
               | MultiVarDecl { vdMods :: Modifiers
                              , vdType :: Maybe Type'
                              , vdMDecls :: [OneVarDecl']
                              }
               | IfStm { ifCond :: Expression'
                       , ifThen :: Statement'
                       , ifElse :: Statement'
                       }
               | BlockStm Block
               | LabeledStm { label :: Text'
                            , lStm :: Statement'
                            }
               | ForStm { forInit :: Maybe ForInit'
                        , forCond :: Maybe Expression'
                        , forUpd :: Maybe [Expression']
                        , forBody :: Statement'
                        }
               | WhileStm { whCond :: Expression'
                          , whBody :: Statement'
                          }
               | DoStm { doCond :: Expression'
                       , doBody :: Statement'
                       }
               | SwitchStm { swOn :: Expression'
                           , swCases :: [Case']
                           , swDef :: [Statement]
                           }
               | ReturnStm Expression'
               | StmExpr Expression
                 deriving (Show, Ord, Eq)

type Case' = AN Case
data Case = Case { cValue :: Expression
                 , cBody :: [Statement]
                 }
            deriving (Show, Ord, Eq)

type ForInit' = AN ForInit
data ForInit = FVDecl { fvdMods :: Modifiers
                      , fvdType :: Maybe Type'
                      , fvdDecls :: [OneVarDecl']
                      }
             | FExps { fExps :: [Expression] }
               deriving (Show, Ord, Eq)

type OneVarDecl' = AN OneVarDecl
data OneVarDecl = OneVarDecl { ovdName :: Text'
                             , ovdInitializer :: Maybe Expression'
                             }
                  deriving (Show, Ord, Eq)

type VarRef' = AN VarRef
data VarRef = VarAccess Text
            | FieldAccess Expression' Text'
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

--infixOps :: IsString a => [(a, InfixOp)]
infixOps :: [(Text, InfixOp)]
infixOps = 
    [("add", Plus), ("subtract", Minus), ("multiply", Mult), ("divide", Div)
    , ("modulo", Mod), ("logicalOr", LOr), ("logicalAnd", LAnd)
    , ("bitOr", BOr), ("bitAnd", BAnd), ("xor", Xor), ("rShift", RShift)
    , ("lShift", LShift), ("rUShift", RUShift)]

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

type TernaryOp' = AN TernaryOp
data TernaryOp = Query
                 deriving (Show, Ord, Eq)

type Expression' = AN Expression
data Expression = Assign VarRef' Expression'
                | OpAssign VarRef' InfixOp' Expression'
                | BinOp InfixOp' Expression' Expression'
                | IntLit Integer
                | StringLit Text
                | FloatLit Double
                | ArrayLit [Expression']
                | PreOp PrefixOp' Expression'
                | PostOp PostfixOp' Expression'
                | InstanceConstruction Type' [Expression']
                | MethodCall Expression' Text' [GenericParam'] [Expression']
                | FunctionCall Text' [GenericParam'] [Expression']
                | TernOp TernaryOp' Expression' Expression'
                | VarRef VarRef
                  deriving (Show, Ord, Eq)

type GenericParam' = AN GenericParam
data GenericParam = GenericParam
                    deriving (Show, Ord, Eq)

type GenericParamDecl' = AN GenericParamDecl
data GenericParamDecl = GenericParamDecl
                        deriving (Show, Ord, Eq)

