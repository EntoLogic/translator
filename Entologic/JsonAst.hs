
{-# LANGUAGE OverloadedStrings #-}

module Entologic.JsonAst where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Applicative((<$>), (<*>), pure)
import Data.Text

import Entologic.AST

data AstJson = AstJson UAST deriving Show

data UAST = UAST Program deriving Show

instance FromJSON Program where
    parseJSON (Array v) = Program <$> mapM parseJSON (V.toList v)

instance FromJSON ProgramEntry where
    parseJSON (Object o) = do
        (String s) <- o .: "Node"
        case s of
          "Function" -> PEFunc <$> parseJSON o
          "ClassDecl" -> PECls <$> parseJSON o

instance FromJSON AstJson where
    parseJSON (Object obj) = AstJson <$> obj .: "uast"

instance FromJSON UAST where
    parseJSON (Object obj) = UAST <$> obj .: "Program"

instance FromJSON Function where
    parseJSON (Object obj) = do
       (String "Function") <- obj .: "Node"
       Function <$> obj .: "Name"
                <*> obj .:? "RetType"
                <*> obj .: "Arguments"
                <*> obj .: "Body"
--                <*> obj .:? "Extra"
                <*> pure Nothing

instance FromJSON Type where
    parseJSON (String s) = return $ StringT s

instance FromJSON ParamDecl where
    parseJSON (String s) = return $ ParamDecl s Nothing
    parseJSON (Object obj) = ParamDecl <$> obj .: "Name"
                                       <*> obj .:? "Type"

instance FromJSON Body where
    parseJSON (Array stms) = Body <$> mapM parseJSON (V.toList stms)

instance FromJSON Statement where
    parseJSON (Object obj) = do
        (String typ) <- obj .: "Node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> StmExpr <$> parseJSON (Object obj)
      where
        parsers = [("VarDecl", varDecl)]

varDecl :: Object -> Parser Statement
varDecl obj = VarDecl <$> obj .: "Type"
                      <*> obj .: "Name"
                      <*> obj .:? "Value"

instance FromJSON Expression where
    parseJSON (Object obj) = do
        (String typ) <- obj .: "Node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> fail $ "Invalid Expression type: " ++ unpack typ
      where
        parsers = [("Assignment", assignment), ("OpAssign", opAssign), ("BinExpr", binExpr), ("IntLit", intLit),
                   ("PrefixOp", prefixOp), ("PostfixOp", PostfixOp)]

assignment obj = Assign <$> obj .: "Variable"
                        <*> obj .: "Value"

opAssign obj = OpAssign <$> obj .: "Variable"
                        <*> obj .: "Op"
                        <*> obj .: "Value"

binExpr obj = BinOp <$> obj .: "Op"
                    <*> obj .: "FirstArg"
                    <*> obj .: "SecondArg"

preOrPostOp const obj = const <$> obj .: "Op"
                              <*> obj .: "Arg"

instance FromJSON PrefixOp where
    parseJSON (String s) = do
        (Just op) <- return $ lookup s ops
        return op
      where
        ops = [("Neg", Neg), ("BInv", BInv), ("PreInc", PreInc), ("PreDec", PreDec)]

instance FromJSON PostfixOp where
    parseJSON (String s) = do
        (Just op) <- return $ lookup s ops
        return op
      where
        ops = [("Inc", PostInc), ("Dec", PostDec)]

preOp = preOrPostOp PreOp
postOp = preOrPostOp PostOp

intLit obj = IntLit . read <$> obj .: "Val"

instance FromJSON VarRef where
    parseJSON (String s) = pure $ StringV s

instance FromJSON InfixOp where
    parseJSON (String s) = do
        (Just op) <- return $ lookup s ops
        return op
      where
        ops = [("Add", Plus), ("Sub", Minus), ("Mult", Mult), ("Div", Div),
               ("Mod", Mod), ("LOr", LOr), ("LAnd", LAnd), ("BOr", BOr),
               ("BAnd", BAnd), ("Xor", Xor), ("RShift", RShift), ("LShift", LShift), ("RUShift", RUShift)]
