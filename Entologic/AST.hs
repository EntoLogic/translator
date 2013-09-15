
{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Entologic.AST where

import Data.Text

class ASTNode a where
    toEng :: (ASTNode a) => a -> String

data NInf = NInf {lineNo :: Int}

data Program = Program [LSAny]


data Type = StringT Text
          | forall a. (ASTNode a) => LSType a


data LSAny = forall a. (ASTNode a) => LSAny a

data Function = Function { fName :: String, fRTyp :: (Maybe Type), fParams :: [ParamDecl],
                           fBody :: Body, fExtra :: (Maybe LSAny) }

data ParamDecl = ParamDecl { pName :: Text, pType :: (Maybe Type) }

data Body = Body [Statement]

data Statement = VarDecl Type Text (Maybe Expression)

data VarRef = StringV Text
            | DottedV [Text]

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

data Expression = Assign VarRef Expression
                | OpAssign VarRef InfixOp Expression
                | BinOp InfixOp Expression Expression
