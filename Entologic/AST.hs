
{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Entologic.AST where

import Data.Text

type DB = Data.Map String String

class ASTNode a where
    toEng :: DB -> a -> String

data NInf = NInf {lineNo :: Int}
            deriving (Show)

data ProgramEntry = PEFunc Function
                  | PECls Class
                  | PEStm Statement
                  deriving (Show)

data Program = Program [ProgramEntry]
               deriving (Show)

data Type = StringT Text
--          | forall a. (ASTNode a, Show a) => LSType a
            deriving (Show)


data LSAny = forall a. (ASTNode a, Show a) => LSAny a

instance Show LSAny where
    show _ = "LSAny"

data Class = Class { cName :: String, cSuperCls :: Maybe Type, cMembers :: [Member] }

data Member = MFunc Function
            | MField Field

data Function = Function { fName :: String, fRTyp :: (Maybe Type), fParams :: [ParamDecl],
                           fBody :: Body, fExtra :: (Maybe LSAny) }
                deriving (Show)

data ParamDecl = ParamDecl { pName :: Text, pType :: (Maybe Type) }
                 deriving (Show)

data Body = Body [Statement]
            deriving (Show)

data Statement = VarDecl Type Text (Maybe Expression)
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
