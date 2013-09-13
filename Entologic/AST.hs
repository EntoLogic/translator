
{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Entologic.AST where

class ASTNode a where
    toEng :: (ASTNode a) => a -> String

data NInf = NInf {lineNo :: Int}

data Program = Program [LSAny]

data Type where
    LSType :: (ASTNode a) => a -> Type


data LSAny = forall a. (ASTNode a) => LSAny a

data Function = Function { fTyp :: (Maybe Type), fParams :: [ParamDecl], fBody :: Body (Maybe LSAny) }

data ParamDecl = ParamDecl Name (Maybe Type)

data Name = SName String

data Body = Body [Statement]

data Statement = VarDecl 
