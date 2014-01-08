
{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , CPP
           #-}

module Entologic.Ast.Json where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector ((!?))
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe (maybeToList)
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.Number (Number(..))

import Text.Read (readMaybe)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad

import Entologic.Ast
import Entologic.Base
import Entologic.Error


readAstFile :: FilePath -> IO UAst
readAstFile path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json

maybeLToList :: Maybe [a] -> [a]
maybeLToList = concat . maybeToList

(.:*) :: FromJSON a => Object -> Text -> Parser [a]
obj .:* name = maybeLToList <$> obj .:? name

area :: Object -> Parser Area
area obj = area' =<< obj .:? "loc"

area' :: Maybe Object -> Parser Area
area' (Nothing) = pure $ Area Nothing Nothing
area' (Just obj) = Area <$> (join <$> obj .:? "start")
                        <*> (join <$> obj .:? "end")

instance FromJSON (Maybe Location) where
    parseJSON (Array arr) = return $ Location <$> (toInt <$> arr !? 0)
                                              <*> (toInt <$> arr !? 1)
      where
        toInt (Number (I i)) = fromIntegral i
        toInt (Number (D d)) = floor d
    parseJSON _ = return Nothing

#define FAIL(typ) parseJSON x = fail $ "Couldn't parse typ from " ++ show x

instance FromJSON UAst where
    parseJSON (Object map) = UAst <$> map .: "Meta"
                                  <*> map .: "Program"
    FAIL(UAst)

instance FromJSON AstMeta where
    parseJSON (Object map) = return AstMeta {-<$> map .: "Language"
                                     <*> map .: "SpokenLanguage" -}
    FAIL(AstMeta)

instance FromJSON Program where
    parseJSON (Array v) = Program <$> mapM parseJSON (V.toList v)
    parseJSON (Object obj) = do
        node <- obj .: "node"
        case s node of
          "Program" -> parseJSON =<< obj .: "contents"
          "CompilationUnit" ->
            CompilationUnit <$> obj .: "package"
                            <*> obj .: "imports"
                            <*> obj .: "declarations"
    FAIL(Program)

instance FromJSON Import where
    FAIL(Import)

instance FromJSON TypeDeclaration where
    FAIL(TypeDeclaration)

instance FromJSON ProgramEntry where
    parseJSON o@(Object map) = do
        s <- map .: "node"
        case s :: String of
          "FuncDecl" -> PEFunc <$> parseJSON o
          _ -> PEStm <$> parseJSON o
    FAIL(ProgramEntry)
--          "ClassDecl" -> PECls <$> parseJSON o


instance FromJSON Function where
    parseJSON (Object obj) = do
--       (String "FuncDecl") <- obj .: "node"
       Function <$> obj .:* "modifiers"
                <*> obj .:? "returnType"
                <*> obj .: "name"
                <*> obj .:* "arguments"
                <*> obj .:* "body"
    FAIL(FuncDecl)

instance FromJSON Type where
    parseJSON (String s) = return $ StringT s
    FAIL(Type)

instance FromJSON ParamDecl where
    parseJSON (String s) = return $ ParamDecl (toAn s) Nothing [] Nothing Nothing
    parseJSON (Object obj) = ParamDecl <$> obj .: "name"
                                       <*> obj .:? "type"
                                       <*> obj .:* "modifiers"
                                       <*> obj .:? "initializer"
                                       <*> pure Nothing
    FAIL(ParamDecl)

instance FromJSON Body where
    parseJSON (Array stms) = Body <$> mapM parseJSON (V.toList stms)
    FAIL(Body)

instance FromJSON Statement where
    parseJSON (Object obj) = do
        typ <- obj .: "node"
        case lookup (typ :: Text) parsers of
          Just f -> f obj
          Nothing -> StmExpr <$> parseJSON (Object obj)
      where
        parsers = [("VarDecl", varDecl)]
    FAIL(Statement)

varDecl :: Object -> Parser Statement
varDecl obj = VarDecl <$> obj .:* "modifiers"
                      <*> obj .:? "type"
                      <*> obj .: "name"
                      <*> obj .:? "initializer"

instance FromJSON Expression where
    parseJSON (Object obj) = do
        typ <- obj .: "node"
        case lookup typ parsers of
          Just f -> f obj
          Nothing -> fail $ "Invalid Expression type: " ++ T.unpack typ
      where
        parsers :: [(Text, Object -> Parser Expression)]
        parsers =
          [("Assignment", assignment), ("OpAssign", opAssign)
          , ("BinaryExpr", binExpr), ("IntLit", intLit), ("PrefixOp", preOp)
          , ("PostfixOp", postOp), ("VarAccess", varRef)
          , ("FieldAccess", varRef), ("StringLit", stringLit)
          , ("FunctionCall", functionCall), ("MethodCall", methodCall)]
    FAIL(Expression)


instance FromJSON VarRef where
    parseJSON (String s) = pure $ VarAccess s
    parseJSON (Object obj) = do
        (String typ) <- obj .: "node"
        case typ of
          "VarAccess" -> VarAccess <$> obj .: "var"
          "FieldAccess" -> FieldAccess <$> obj .: "obj" <*> obj .: "field"
    FAIL(VarRef)

instance FromJSON a => FromJSON (AN a) where
    parseJSON obj@(Object map) = do
        node <- map .: "node"
        case s node of
          "Unknown" -> tupleM (Unknown <$> map .:? "err" .!= "Unknown error"
                              , return $ Area Nothing Nothing)
          _ -> tupleM (Node <$> parseJSON obj, area map)
    parseJSON obj = tupleM (Node <$> parseJSON obj
                           , return $ Area Nothing Nothing)

an :: (Value -> Parser a) -> Value -> Parser (AN a)
an f obj@(Object map) = tupleM (Node <$> f obj, area map)

an' :: (Object -> Parser a) -> Object -> Parser (AN a)
an' f obj = tupleM (Node <$> f obj, area obj)

assignment obj = Assign <$> obj .: "variable"
                        <*> obj .: "value"

opAssign obj = OpAssign <$> obj .: "variable"
                        <*> obj .: "op"
                        <*> obj .: "value"

binExpr obj = BinOp <$> obj .: "op"
                    <*> obj .: "left"
                    <*> obj .: "right"

preOrPostOp :: FromJSON a => (AN a -> Expression' -> Expression) -> Object
                                -> Parser Expression
preOrPostOp const obj = const <$> obj .: "op"
                              <*> obj .: "operand"

varRef obj = VarRef <$> parseJSON (Object obj)
stringLit obj = StringLit <$> obj .: "value"

functionCall obj = FunctionCall <$> obj .: "name"
                                <*> obj .:* "genericParameters"
                                <*> obj .:* "arguments"

methodCall obj = MethodCall <$> obj .: "object"
                            <*> obj .: "name"
                            <*> obj .:* "genericParameters"
                            <*> obj .:* "arguments"

instance FromJSON PrefixOp where
    parseJSON (String s) = mFromJust $ lookup s ops
      where
        ops = [("Neg", Neg), ("BInv", BInv), ("PreInc", PreInc)
              , ("PreDec", PreDec)]
    FAIL(PrefixOp)

instance FromJSON PostfixOp where
    parseJSON (String s) = mFromJust $ lookup s ops
      where
        ops = [("Inc", PostInc), ("Dec", PostDec)]
    FAIL(PostfixOp)

preOp = preOrPostOp PreOp
postOp = preOrPostOp PostOp

intLit obj = (fmap IntLit . mFromJust . readMaybe) =<< obj .: "value"


instance FromJSON InfixOp where
    parseJSON (String s) = mFromJust $ lookup s infixOps
    FAIL(InfixOp)

instance FromJSON Modifier where
    parseJSON (Object obj) = obj .: "modifier"
    parseJSON (String s) = mFromJust $ lookup s mods
      where
        mods =
          [("public", Public), ("private", Private) , ("protected", Protected)
          , ("static", Static), ("const", Const), ("final", Final)
          , ("abstract", Abstract), ("transient", Transient)
          , ("volatile", Volatile), ("synchronized", Synchronized)
          , ("strictfp", StrictFP)]
    FAIL(Modifier)

instance FromJSON GenericParam where
    parseJSON _ = return GenericParam
