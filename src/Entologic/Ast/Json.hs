
{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , CPP
           #-}

module Entologic.Ast.Json where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import Data.Vector ((!?))
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe (maybeToList, fromJust)
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.Number (Number(..))

import Text.Read (readMaybe)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad

import Entologic.Ast
import Entologic.Base
import Entologic.Error
import Entologic.Translate


readAstFile :: FilePath -> IO UAst
readAstFile path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json

maybeLToList :: Maybe [a] -> [a]
maybeLToList = concat . maybeToList

(.:*) :: FromJSON a => Object -> Text -> Parser [a]
obj .:* name = maybeLToList <$> obj .:? name

(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
(.=?) t = fmap (t .=)

(?:) :: Maybe a -> [a] -> [a]
(?:) Nothing = id
(?:) (Just x) = (x:)
infixr 5 ?:

area :: Object -> Parser Area
area obj = area' =<< obj .:? "loc"

area' :: Maybe Object -> Parser Area
area' (Nothing) = pure $ Area Nothing Nothing
area' (Just obj) = Area <$> (join <$> obj .:? "start")
                        <*> (join <$> obj .:? "end")

instance ToJSON Area where
    toJSON (Area start end) =
        case (start, end) of
          (Nothing, Nothing) -> Null
          _ -> object $ "start" .=? start ?: "end" .=? end ?: []

instance ToJSON Location where
    toJSON (Location l c) = toJSON [l, c]

jsonName node = "node" .= name node

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

instance ToJSON UAst where
    toJSON (UAst meta prog) = object [ "Meta" .= meta, "Program" .= prog ]

instance FromJSON AstMeta where
    parseJSON _ = return AstMeta {-<$> map .: "Language"
                                     <*> map .: "SpokenLanguage" -}
    FAIL(AstMeta)

instance ToJSON AstMeta where
    toJSON _ = Null

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

instance ToJSON Program where
    toJSON (Program contents) = toJSON contents
    toJSON node@(CompilationUnit pkg imports decls) =
        object [ "package" .= pkg, "imports" .= imports
               , "declarations" .= decls, jsonName node ]

instance FromJSON Import where
    FAIL(Import)

instance ToJSON Import where
    toJSON = error "toJSON Import" -- TODO

instance FromJSON TypeDeclaration where
    parseJSON o@(Object obj) = do
        name <- obj .: "node"
        case name :: String of
            "ClassDecl" -> TDCls <$> parseJSON o
    FAIL(TypeDeclaration)

instance ToJSON TypeDeclaration where
    toJSON (TDCls cls) = toJSON cls -- TODO

instance FromJSON ProgramEntry where
    parseJSON o@(Object map) = do
        s <- map .: "node"
        case s :: String of
          "FuncDecl" -> PEFunc <$> parseJSON o
          "ClassDecl" -> PECls <$> parseJSON o
          _ -> PEStm <$> parseJSON o
    FAIL(ProgramEntry)

instance ToJSON ProgramEntry where
    toJSON (PEFunc f) = toJSON f
    toJSON (PECls c) = toJSON c
    toJSON (PEStm s) = toJSON s

instance ToJSON Class where
    toJSON node@(Class mods name gParams super interfs mems) =
        object $ "modifiers" .= mods : "name" .= name
               : "genericParameters" .= gParams : "superClass" .=? super
               ?: "interfaces" .= interfs : "members" .= mems : [jsonName node]

instance FromJSON Class where
    parseJSON (Object obj) = Class <$> obj .:* "modifiers"
                                   <*> obj .: "name"
                                   <*> obj .:* "genericParameters"
                                   <*> obj .:? "superClass"
                                   <*> obj .:* "interfaces"
                                   <*> obj .:* "members"

instance FromJSON InClassDecl where
    parseJSON o@(Object obj) = do
        name <- obj .: "node"
        case name :: String of
            "InitBlock" -> InitBlock <$> obj .: "contents"
            "StaticInitBlock" -> StaticInitBlock <$> obj .: "contents"
            _ -> MemberDecl <$> parseJSON o

instance ToJSON InClassDecl where
    toJSON node@(MemberDecl member) = toJSON member
    toJSON node@(InitBlock block) =
        object [ "contents" .= block, jsonName node ]
    toJSON node@(StaticInitBlock block) = object [ "contents" .= block
                                            , jsonName node ]

instance ToJSON Member where
    toJSON (MFunc func) = toJSON func
    toJSON (MField field) = toJSON field

instance FromJSON Member where
    parseJSON o@(Object obj) = do
        name <- obj .: "node"
        case name :: String of
            "FuncDecl" -> MFunc <$> parseJSON o
            "FieldDecl" -> MField <$> parseJSON o
    

instance FromJSON Function where
    parseJSON (Object obj) = do
--       (String "FuncDecl") <- obj .: "node"
       Function <$> obj .:* "modifiers"
                <*> obj .:? "returnType"
                <*> obj .: "name"
                <*> obj .:* "arguments"
                <*> obj .:? "body"
    FAIL(FuncDecl)

instance ToJSON Function where
    toJSON node@(Function mods ret name args body) = do
        object $ "modifiers" .= mods : "returnType" .=? ret ?: "name" .= name
               : "arguments" .= args : "body" .=? body ?: [jsonName node]

instance FromJSON Field where
    parseJSON _ = return Field

instance ToJSON Field where
    toJSON f = Null

instance FromJSON Type where
    parseJSON (String s) = return $ StringT s
    parseJSON (Object obj) = do
        name <- obj .: "node"
        case name :: String of
            "ClassType" -> ClassType <$> obj .: "parts"
            "ArrayType" -> ArrayType <$> obj .: "elementType"
    FAIL(Type)

instance FromJSON ClassTypePart where
    parseJSON (Object obj) = ClassTypePart <$> obj .: "name"
                                           <*> obj .:* "genericParameters"

instance FromJSON (Text', [GenericParam]) where
    parseJSON (Object obj) = tupleM (obj .: "name", obj .: "genericParameters")

instance ToJSON Type where
    toJSON (StringT s) = toJSON s
    toJSON node@(ClassType parts) =
        object [ "dottedParts" .= parts, jsonName node ]
    toJSON node@(ArrayType elem) =
        object [ "elementType" .= elem, jsonName node ]

instance ToJSON ClassTypePart where
    toJSON node@(ClassTypePart name gParams) =
        object [ "name" .= name, "genericParameters" .= gParams, jsonName node ]

instance FromJSON ParamDecl where
    parseJSON (String s) = return $ ParamDecl (toAn s) Nothing [] Nothing   
                                    Nothing
    parseJSON (Object obj) = ParamDecl <$> obj .: "name"
                                       <*> obj .:? "type"
                                       <*> obj .:* "modifiers"
                                       <*> obj .:? "initializer"
                                       <*> pure Nothing
    FAIL(ParamDecl)

instance ToJSON ParamDecl where
    toJSON node@(ParamDecl name typ mods init _) =
        object $ "name" .= name : "type" .=? typ ?: "modifiers" .= mods
               : "initializer" .=? init ?: [jsonName node]

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

instance ToJSON Statement where
    toJSON (StmExpr e) = toJSON e
    toJSON node@(VarDecl mods typ name init) =
        object $ "modifiers" .= mods : "type" .=? typ ?: "name" .= name
               : "initializer" .=? init ?: [jsonName node]

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
          [ ("Assignment", assignment), ("OpAssign", opAssign)
          , ("BinaryExpr", binExpr), ("IntLit", intLit), ("PrefixOp", preOp)
          , ("PostfixOp", postOp), ("VarAccess", varRef)
          , ("FieldAccess", varRef), ("StringLit", stringLit)
          , ("ArrayLit", arrayLit), ("FunctionCall", functionCall)
          , ("MethodCall", methodCall) ]
    FAIL(Expression)

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

arrayLit obj = ArrayLit <$> obj .: "contents"

functionCall obj = FunctionCall <$> obj .: "name"
                                <*> obj .:* "genericParameters"
                                <*> obj .:* "arguments"

methodCall obj = MethodCall <$> obj .: "object"
                            <*> obj .: "name"
                            <*> obj .:* "genericParameters"
                            <*> obj .:* "arguments"

instance ToJSON Expression where
    toJSON node@(Assign var val) = object [ "variable" .= var, "value" .= val
                                     , jsonName node]
    toJSON node@(OpAssign var op val) = object [ "variable" .= var, "op" .= op
                                          , "value" .= val, jsonName node ]
    toJSON node@(BinOp op l r) = object [ "op" .= op, "left" .= l, "right" .= r
                                   , jsonName node ]
    toJSON node@(VarRef vr) = toJSON vr
    toJSON node@(StringLit s) = object [ "value" .= s, jsonName node ]
    toJSON node@(ArrayLit a) = object [ "contents" .= a, jsonName node ]
    toJSON node@(FunctionCall name gParams args) =
        object [ "name" .= name, "genericParameters" .= gParams
               , "arguments" .= args, jsonName node ]
    toJSON node@(MethodCall obj name gParams args) =
        object [ "name" .= name, "genericParameters" .= gParams
               , "arguments" .= args, "object" .= obj, jsonName node ]

instance FromJSON VarRef where
    parseJSON (String s) = pure $ VarAccess s
    parseJSON (Object obj) = do
        (String typ) <- obj .: "node"
        case typ of
          "VarAccess" -> VarAccess <$> obj .: "var"
          "FieldAccess" -> FieldAccess <$> obj .: "obj" <*> obj .: "field"
    FAIL(VarRef)

instance ToJSON VarRef where
    toJSON node@(VarAccess var) = object [ "var" .= var, jsonName node ]
    toJSON node@(FieldAccess obj var) = object [ "obj" .= obj, "var" .= var
                                                  , jsonName node ]

instance FromJSON a => FromJSON (AN a) where
    parseJSON obj@(Object map) = do
        node <- map .: "node"
        case s node of
          "Unknown" -> tupleM (Unknown <$> map .:? "err" .!= "Unknown error"
                              , return $ Area Nothing Nothing)
          _ -> tupleM (Node <$> parseJSON obj, area map)
    parseJSON obj = tupleM (Node <$> parseJSON obj
                           , return $ Area Nothing Nothing)

instance ToJSON a => ToJSON (AN a) where
    toJSON (Unknown err, a) = areaTo a $ object [ "node" .= s "Unknown"
                                                , "err" .= err ]
    toJSON (Node n, a) = areaTo a $ toJSON n

areaTo :: Area -> Value -> Value
areaTo a (Object obj) = Object $ H.insert "loc" (toJSON a) obj
areaTo a x = x

an :: (Value -> Parser a) -> Value -> Parser (AN a)
an f obj@(Object map) = tupleM (Node <$> f obj, area map)

an' :: (Object -> Parser a) -> Object -> Parser (AN a)
an' f obj = tupleM (Node <$> f obj, area obj)


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
    parseJSON (Object obj) = obj .: "op"
    parseJSON (String s) = mFromJust $ lookup s infixOps
    FAIL(InfixOp)

instance ToJSON InfixOp where
    toJSON op = object [ "op" .= fromJust (M.lookup op ops) ]
      where ops = M.fromList . map swap $ infixOps

instance FromJSON Modifier where
    parseJSON (Object obj) = obj .: "modifier"
    parseJSON (String s) = mFromJust $ lookup s modifiers
    FAIL(Modifier)

instance ToJSON Modifier where
    toJSON mod = object [ "modifier" .= fromJust (M.lookup mod mods) ]
      where mods = M.fromList . map swap $ modifiers

instance FromJSON GenericParam where
    parseJSON _ = return GenericParam

instance FromJSON GenericParamDecl where
    parseJSON _ = return GenericParamDecl

instance ToJSON GenericParam where
    toJSON _ = Null

instance ToJSON GenericParamDecl where
    toJSON _ = Null
