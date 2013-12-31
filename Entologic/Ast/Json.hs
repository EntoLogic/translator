
{-# LANGUAGE OverloadedStrings,
             FlexibleInstances#-}

module Entologic.Ast.Json where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Text
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.Number (Number(..))

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad

import Entologic.Ast
import Entologic.Base



readAstFile :: FilePath -> IO UAst
readAstFile path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json

area :: Object -> Parser Area
area obj = area' =<< obj .:? "location"

area' :: Maybe Object -> Parser Area
area' (Nothing) = pure $ Area Nothing Nothing
area' (Just obj) = Area <$> (join <$> obj .:? "start") <*> (join <$> obj .:? "end")

instance FromJSON (Maybe Location) where
    parseJSON (Array arr) = return $ Location <$> (toInt <$> arr !? 0) <*> (toInt <$> arr !? 1)
      where
        toInt (Number (I i)) = fromIntegral i
    parseJSON _ = return Nothing

instance FromJSON UAst where
    parseJSON (Object map) = UAst <$> map .: "Meta"
                                  <*> map .: "Program"

instance FromJSON AstMeta where
    parseJSON (Object map) = return AstMeta {-<$> map .: "Language"
                                     <*> map .: "SpokenLanguage" -}

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
--    parseJSON (String s) = return $ ParamDecl s Nothing
    parseJSON (Object obj) = ParamDecl <$> tupleM (obj .: "Name", area obj)
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
        parsers :: [(Text, Object -> Parser Expression)]
        parsers = [("Assignment", assignment), ("OpAssign", opAssign), ("BinaryExpr", binExpr), ("IntLit", intLit),
                   ("PrefixOp", preOp), ("PostfixOp", postOp)]

instance FromJSON a => FromJSON (AN a) where
    parseJSON obj@(Object map) = tupleM (parseJSON obj, area map)
    parseJSON obj = tupleM (parseJSON obj, return $ Area Nothing Nothing)

an :: (Value -> Parser a) -> Value -> Parser (AN a)
an f obj@(Object map) = tupleM (f obj, area map)

an' :: (Object -> Parser a) -> Object -> Parser (AN a)
an' f obj = tupleM (f obj, area obj)

assignment obj = Assign <$> obj .: "Variable"
                        <*> obj .: "Value"

opAssign obj = OpAssign <$> obj .: "Variable"
                        <*> obj .: "Op"
                        <*> obj .: "Value"

binExpr obj = BinOp <$> obj .: "op"
                    <*> obj .: "left"
                    <*> obj .: "right"

preOrPostOp :: FromJSON a => (AN a -> Expression' -> Expression) -> Object -> Parser Expression
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
