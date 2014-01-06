
{-# LANGUAGE OverloadedStrings
           , ExtendedDefaultRules
           , GeneralizedNewtypeDeriving
           , RankNTypes
           , TypeSynonymInstances
           , FlexibleInstances
           , OverlappingInstances
           , UndecidableInstances
           , CPP
           #-}

module Entologic.Translate where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Maybe (fromJust, mapMaybe)
import Data.List (intersperse)
import Data.Tuple (swap)
import qualified Data.Traversable as TV

import qualified Database.MongoDB as DB

import Entologic.Base
import Entologic.Ast
import Entologic.Phrase
import Entologic.Output
import Entologic.Error

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Category ((<<<), (>>>))

bracket x y = T.cons x . flip T.snoc y

bracket' :: Char -> Char -> [OutputClause] -> [OutputClause]
bracket' x y = cons (OCString $ T.pack [x]) . flip snoc (OCString $ T.pack [y])

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
infixl 4 <$$>



chooseM :: Functor m => m Bool -> a -> a -> m a
chooseM cond x y = cond <$$> \c -> if c then x else y

chooseL :: (MonadState s m, Functor m) => Getter s Bool -> a -> a -> m a
chooseL getter = chooseM (use getter)

choose :: Bool -> a -> a -> a
choose cond x y = if cond then x else y

localS :: MonadState s m => (s -> s) -> m a -> m a
localS mod action = do
    before <- get
    put $ mod before
    a <- action
    put before
    return a

areaUseful :: Area -> Bool
areaUseful (Area Nothing Nothing) = False
areaUseful _ = True

instance Variable Bool where
    present = id
    comparison = fromEnum
    inPhrase = return . (:[]) . OCString . T.pack . show

instance Variable Int where
    present = (>0)
    comparison = id
    inPhrase = return . (:[]) . OCString . T.pack . show

{-
class ShowOrVariable a where
     showOrInPhrase :: a -> TL String

instance Show a => ShowOrVariable a where
    showOrInPhrase = return . show

instance Variable a => ShowOrVariable a where
    showOrInPhrase = inPhrase
-}


instance Variable a => Variable [a] where
    present = (>0) . length
    comparison = length
    inPhrase list = (listify' =<< concat <$> mapM inPhrase list)

{-
instance Show a => Variable [a] where
    present = (>0) . length
    comparison = length
    inPhrase = fmap OCString . listify . map show
-}

instance Variable String where
    present = (>0) . length
    comparison = length
    inPhrase = return . (:[]) . OCString . T.pack

instance Variable Text where
    present = (>0) . T.length
    comparison = T.length
    inPhrase = return . (:[]) . OCString

instance Variable Text' where
    present (t, _) = present t
    comparison (t, _) = comparison t
    inPhrase (t, a) = case t of
        Unknown err -> return [OCString err]
        Node t' ->
            if not $ areaUseful a
            then return [OCString t']
            else return . (:[]) . OCNode $ OutputNode "Identifier" [OCString t']
                                                        False a ""

instance Variable OutputClause where 
    inPhrase = return . (:[])

    present (OCString x) = present x
    present (OCNodes xs) = length xs > 0
    present _ = True

    comparison (OCString x) = comparison x
    comparison (OCNodes xs) = length xs
    comparison _ = 1

instance Variable [OutputClause] where
    inPhrase = return
    present = foldl (||) False . map present
    comparison = foldl (+) 0 . map comparison

instance Variable a => Variable (Maybe a) where
    inPhrase Nothing = return []
    inPhrase (Just v) = inPhrase v
    present Nothing = False
    present (Just v) = present v
    comparison Nothing = 0
    comparison (Just v) = comparison v

nodeErr :: Text -> OutputNode
nodeErr err = OutputNode "Unknown" [OCString err] False (Area Nothing Nothing)
                         "error"

instance Variable a => Variable (UK a) where
    inPhrase (Unknown err) = return [OCNode $ nodeErr err]
    inPhrase (Node v) = inPhrase v
    present (Unknown err) = False
    present (Node n) = present n
    comparison (Unknown err) = 0
    comparison (Node n) = comparison n

{-
instance AstNode a => Variable a where
    present = const True
    comparison = const 1
    inPhrase = translate
-}

initLast :: [a] -> (Maybe a, [a])
initLast [x] = (Just x, [])
initLast (x:xs) = (x:) <$> initLast xs
initLast [] = (Nothing, [])

{-
listify :: [Text] -> TL Text
listify xs = let (last', init) = initLast xs
             in case last' of
                Nothing -> return ""
                Just last ->
                  return $ (T.intercalate ", " init) `T.append`
                    (" and " `T.append` last)
-}

listify' :: [OutputClause] -> TL [OutputClause]
listify' xs = let (last', init) = initLast xs
              in case last' of
                Nothing -> return []
                Just last ->
                  return $ (intersperse (OCString ", ") init) ++
                                ([(OCString " and ")] ++ [last])

{-
instance AstNode a => AstNode (Maybe a) where
    name Nothing = "Nothing"
    name (Just x) = name x

    translate' (Nothing, _) = return []
    translate' (Just x, area) = translate' (x, area)
-}

instance AstNode Program where
    name = const "Program"
    {-
    translate' (node, area) = do
        contents <- OCNodes . concat <$>
                        (mapM (fmap (fmap ocNode) . translate) $ pEntries node)
        let vars = M.fromList [("contents", AV contents)]
        defTrans node area vars
    -}
    translate' (node, area) = do
        contents <- mapM translate $ pEntries node
        return $ concat contents
        -- TODO

instance AstNode ProgramEntry where
    name (PEStm s) = name s
    name (PEFunc f) = name f
    translate' (PEStm s, area) = translate' (s, area)
    translate' (PEFunc f, area) = translate' (f, area)
    -- TODO: Other ProgramEntry types

maybeTranslate :: AstNode a => Maybe (AN a) -> TL (Maybe [OutputClause])
maybeTranslate = TV.sequence . fmap translate

#define NAME(node) name (node {}) = "node"

instance AstNode Function where
    name = const "FuncDecl"
    translate' (node@(Function modifiers typ name arguments body), area) = do
        mods <- mapM translate modifiers
        typ' <- maybeTranslate typ
        args <- mapM translate arguments
        body' <- mapM translate body
        let vars = M.fromList [ ("modifiers", AV mods), ("returnType", AV typ')
                              , ("name", AV name), ("arguments", AV args)
                              , ("body", AV body') ]
        defTrans node area vars

instance AstNode ParamDecl where
    name = const "ParamDecl"
    translate' (node@(ParamDecl name typ def), area) = do
        typ' <- maybeTranslate typ
        def' <- maybeTranslate def
        let vars = M.fromList [ ("name", AV name), ("type", AV typ')
                              , ("default", AV def') ]
        defTrans node area vars

instance AstNode Statement where
    name (StmExpr _) = "StmExpr"
    NAME(VarDecl)
    NAME(MultiVarDecl)
    NAME(IfStm)
    NAME(ForStm)
    NAME(WhileStm)
    NAME(DoStm)
    NAME(ReturnStm)
    NAME(SwitchStm)
    NAME(BlockStm)

    translate' (StmExpr e, area) = translate' (e, area)

    translate' (node@(VarDecl modifiers typ name initializer), area) = do
        mods <- mapM translate modifiers
        typ' <- maybeTranslate typ
        init <- runSubExpr node . TV.sequence $ translate <$> initializer
        let vars = M.fromList
                     [("modifiers", AV mods), ("type", AV typ')
                     , ("initializer", AV init), ("name", AV name)]
        defTrans node area vars

    translate' (node@(MultiVarDecl modifiers typ decls), area) = do
        mods <- mapM translate modifiers
        typ' <- maybeTranslate typ
        decls <- mapM translate decls
        let vars = M.fromList
                     [("modifiers", AV mods), ("type", AV typ')
                     , ("declarations", AV decls)]
        defTrans node area vars

    -- TODO: Other Statement types

instance AstNode OneVarDecl where
    name = const "OneVarDecl"
    translate' (node@(OneVarDecl name initializer), area) = do
        init <- maybeTranslate initializer
        let vars = M.fromList [("name", AV name), ("initializer", AV init)]
        defTrans node area vars
        

instance AstNode Expression where
    name (BinOp {}) = "BinaryExpr"
    name (IntLit {}) = "IntLit"
    name (StringLit {}) = "StringLit"
    name (PreOp {}) = "PrefixExpr"
    name (PostOp {}) = "PostfixExpr"
    name (Assign {}) = "Assignment"
    name (InstanceConstruction {}) = "InstanceConstruction"
    name (MethodCall {}) = "MethodCall"
    name (FunctionCall {}) = "FunctionCall"

    translate' (node@(Assign varRef expression), area) = do
        let rse = runSubExpr node
        var <- translate varRef
        expr <- rse $ translate expression
        subexpr <- inSubExpr
        let vars = M.fromList [ ("variable", AV var), ("expression", AV expr)
                              , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(OpAssign varRef infixOp expression), area) = do
        let rse = runSubExpr node
        var <- rse $ translate varRef
        op <- rse $ translate infixOp
        expr <- rse $ translate expression
        subexpr <- inSubExpr
        let vars = M.fromList [ ("variable", AV var), ("expression", AV expr)
                              , ("op", AV op), ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(BinOp op lexpr rexpr), area) = do
        let rse = runSubExpr node
        tOp <- translate op
        left <- rse $ translate lexpr
        right <- rse $ translate rexpr
        subexpr <- inSubExpr
        let vars = M.fromList [ ("operation", AV tOp), ("left", AV left)
                              , ("right", AV right)
                              , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(IntLit val), area) = do
        result node area ((:[]) . OCString . T.pack $ show val) ""

    translate' (node@(StringLit val), area) = do
        result node area ((:[]) . OCString . T.pack $ show val) ""

    translate' (node@(PreOp op expression), area) = do
        tOp <- translate op
        expr <- runSubExpr node $ translate expression
        subexpr <- inSubExpr
        let vars = M.fromList [("operation", AV tOp), ("expression", AV expr)
                             , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(PostOp op expression), area) = do
        tOp <- translate op
        expr <- runSubExpr node $ translate expression
        subexpr <- inSubExpr
        let vars = M.fromList [("operation", AV tOp), ("expression", AV expr)
                             , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(InstanceConstruction typ arguments), area) = do
        let rse = runSubExpr node
        typ' <- rse $ translate typ
        args <- rse $ mapM translate arguments
        subexpr <- inSubExpr
        let vars = M.fromList [("type", AV typ'), ("arguments", AV args)
                              , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(MethodCall object method genericParams arguments), area) =
      do
        let rse = runSubExpr node
        obj <- rse $ translate object
        gParams <- rse $ mapM translate genericParams
        args <- rse $ mapM translate arguments
        subexpr <- inSubExpr
        let vars = M.fromList [("object", AV obj), ("methodName", AV method)
                    , ("genericParameters", AV gParams), ("arguments", AV args)
                    , ("subexpression", AV subexpr)]
        defTrans node area vars

    translate' (node@(FunctionCall function genericParams arguments), area) = do
        let rse = runSubExpr node
        gParams <- rse $ mapM translate genericParams
        args <- rse $ mapM translate arguments
        subexpr <- inSubExpr
        let vars = M.fromList [("functionName", AV function)
                    , ("genericParameters", AV gParams), ("arguments", AV args)
                    , ("subexpression", AV subexpr)]
        defTrans node area vars


instance AstNode VarRef where
    NAME(VarAccess)
    NAME(FieldAccess)

    translate' (node@(VarAccess var), area) = do
        let vars = M.fromList [("varName", AV var)]
        defTrans node area vars

    translate' (node@(FieldAccess obj field), area) = do
        obj' <- runSubExpr node $ translate obj
        let vars = M.fromList [("fieldName", AV field), ("object", AV obj')]
        defTrans node area vars
    
instance AstNode Type where
    translate' = undefined


runSubExpr :: AstNode n => n -> TL a -> TL a
runSubExpr node = localS (sInSubExpr .~ True <<< sPrevExprType .~ name node)

inSubExpr :: TL Bool
inSubExpr = use sInSubExpr

parens :: AstNode a => a ->  TL ([OutputClause] -> [OutputClause])
parens node = chooseM needsParens (bracket' '(' ')') id
    where
      needsParens = (&&) <$> use sInSubExpr <*> ((name node ==) <$>
                                                    use sPrevExprType)

english :: TLInfo -> TLInfo
english = tlSLang .~ "en"

toText :: OutputClause -> Text
toText (OCString t) = t
toText (OCNode n) = T.concat . map toText $ n ^. oClauses
toText (OCNodes n) = T.concat . concat . map (map toText . (^. oClauses)) $ n

webTranslate :: [OutputClause] -> TL [OutputClause]
webTranslate clauses = do
    let text = T.concat $ map toText clauses
    return []

defTrans :: AstNode a => a -> Area -> AnyVariables -> TL [OutputClause]
defTrans node area vars = do
    clauses <- getClauses $ name node
    parens' <- parens node
    case clauses of
      Just (clauses', id) -> do
        clauses <- insertClauses clauses' vars
        result node area (parens' clauses) $ T.pack $ show id
      Nothing -> webTranslate =<< local english (translate' (node, area))

result :: AstNode a => a -> Area -> [OutputClause] -> Text
                         -> TL [OutputClause]
result node area translation id =
    return . (:[]) . OCNode $ OutputNode (name node) translation False area id

        
infixOpNames = M.fromList . map swap $ infixOps
instance AstNode InfixOp where
    name node = fromJust $ M.lookup node infixOpNames
    translate' (node, area) = defTrans node area M.empty

instance AstNode PrefixOp where
    name Neg = "negate"
    name BInv = "bitwiseInvert"
    name PreInc = "preIncrement"
    name PreDec = "preDecrement"

    translate' (node, area) = defTrans node area M.empty

instance AstNode PostfixOp where
    name PostInc = "postIncrement"
    name PostDec = "posDecrement"

    translate' (node, area) = defTrans node area M.empty

modNames = M.fromList . map swap $ modifiers
instance AstNode Modifier where
    name node = fromJust $ M.lookup node modNames
    translate' (node, area) = defTrans node area M.empty
    

instance AstNode GenericParam where
    name = const "GenericParameter"

    translate' = const $ return []
