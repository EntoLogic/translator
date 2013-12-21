
{-# LANGUAGE OverloadedStrings #-}

module Entologic.Ast.Json where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Applicative((<$>), (<*>), pure)
import Data.Text
import qualified Data.ByteString.Lazy as L

import Entologic.Ast

data UAst = UAst {uMeta :: AstMeta, uProg :: Program} deriving Show


readAst :: FilePath -> IO UAst
readAst path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json


instance FromJSON UAst where
    parseJSON (Object map) = UAst <$> map .: "Meta"
                                  <*> map .: "Program"

instance FromJSON AstMeta where
    parseJSON (Object map) = AstMeta <$> map .: "Language"
                                     <*> map .: "SpokenLanguage"

instance FromJSON Program where
    parseJSON (Array v) = Program <$> mapM parseJSON (V.toList v)

instance FromJSON ProgramEntry where
    parseJSON o@(Object map) = do
        (String s) <- map .: "node"
        case s of
          "Function" -> PEFunc <$> parseJSON o
          _ -> PEStm <$> parseJSON o
--          "ClassDecl" -> PECls <$> parseJSON o


instance FromJSON Function where
    parseJSON (Object obj) = do
       (String "Function") <- obj .: "node"
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
        (String typ) <- obj .: "node"
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
        (String typ) <- obj .: "node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> fail $ "Invalid Expression type: " ++ unpack typ
      where
        parsers = [("Assignment", assignment), ("OpAssign", opAssign), ("BinaryExpr", binExpr), ("IntLit", intLit),
                   ("PrefixOp", preOp), ("PostfixOp", postOp)]

assignment obj = Assign <$> obj .: "Variable"
                        <*> obj .: "Value"

opAssign obj = OpAssign <$> obj .: "Variable"
                        <*> obj .: "Op"
                        <*> obj .: "Value"

binExpr obj = BinOp <$> obj .: "op"
                    <*> obj .: "left"
                    <*> obj .: "right"

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

intLit obj = IntLit . read <$> obj .: "value"

instance FromJSON VarRef where
    parseJSON (String s) = pure $ StringV s

instance FromJSON InfixOp where
    parseJSON (String s) = do
        (Just op) <- return $ lookup s ops
        return op
      where
        ops = [("add", Plus), ("sub", Minus), ("multiply", Mult), ("divide", Div),
               ("modulo", Mod), ("logicalOr", LOr), ("logicalAnd", LAnd), ("bitOr", BOr),
               ("bitAnd", BAnd), ("xor", Xor), ("rShift", RShift), ("lShift", LShift), ("rUShift", RUShift)]
