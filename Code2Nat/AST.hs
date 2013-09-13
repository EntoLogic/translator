
{-# LANGUAGE GADTs #-}

module Code2Nat.AST where

class ASTNode a where
    toEng :: (ASTNode a) => a -> String


data Program where
    LSProg :: (ASTNode a) => a -> Program

data Type where
    LSType :: (ASTNode a) => a -> Type

data LSAny where
    LSAny :: (ASTNode a) => a -> LSAny

data Function = Function (Maybe Type) [ParamDecl] Body (Maybe LSAny)

data ParamDecl = ParamDecl Name (Maybe Type)

data Name = SName String

data Body = Body Int
