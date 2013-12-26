
{-# LANGUAGE OverloadedStrings,
             ExtendedDefaultRules,
             GeneralizedNewtypeDeriving,
             RankNTypes,
             TypeSynonymInstances,
             FlexibleInstances,
             OverlappingInstances,
             UndecidableInstances#-}

module Entologic.Translate where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Maybe (fromJust, mapMaybe)

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


result node area translation = return . OCNode $ OutputNode (name node) translation
                            False area

on2Text (OutputNode _ ((OCString t):_) _ _) = t
oc2Text (OCNode x) = on2Text x

instance Variable Bool where
    present = id
    comparison = fromEnum
    inPhrase = OCString . T.pack . show

instance Variable Int where
    present = (>0)
    comparison = id
    inPhrase = OCString . T.pack . show

{-
class ShowOrVariable a where
     showOrInPhrase :: a -> TL String

instance Show a => ShowOrVariable a where
    showOrInPhrase = return . show

instance Variable a => ShowOrVariable a where
    showOrInPhrase = inPhrase
-}

{-
instance Variable a => Variable [a] where
    present = (>0) . length
    comparison = length
    inPhrase list = fmap OCString (listify =<< mapM inPhrase list)
-}

{-
instance Show a => Variable [a] where
    present = (>0) . length
    comparison = length
    inPhrase = fmap OCString . listify . map show
-}

instance Variable String where
    present = (>0) . length
    comparison = length
    inPhrase = OCString . T.pack

instance Variable Text where
    present = (>0) . T.length
    comparison = T.length
    inPhrase = OCString

instance Variable OutputClause where 
    inPhrase = id

    present (OCString x) = present x
    present (OCNodes xs) = length xs > 0
    present _ = True

    comparison (OCString x) = comparison x
    comparison (OCNodes xs) = length xs
    comparison _ = 1

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

listify :: [Text] -> TL Text
listify xs = let (last', init) = initLast xs
             in case last' of
                Nothing -> return ""
                Just last ->
                  return $ (T.intercalate ", " init) `T.append` (" and " `T.append` last)

instance AstNode Program where
    name = const "program"
    {-translate (node, area) = do
        clauses <- getClauses "program" 
        contents <- OCNodes <$> (mapM (fmap ocNode . translate) $ pEntries node)
        let vars = M.fromList [("contents", contents)]
            conds = []
            cconds = M.empty
            translation = insertClauses clauses vars conds cconds
        return . OCNode $ OutputNode (name node) translation False
                                     (Area Nothing Nothing)-}
    translate (node, area) = do
        clauses <- getClauses "program" 
        contents <- OCNodes <$> (mapM (fmap ocNode . translate) $ pEntries node)
        let vars = M.fromList [("contents", AV contents)]
            translation = insertClauses' clauses vars
        return . OCNode $ OutputNode (name node) translation False
                                     area

instance AstNode ProgramEntry where
    name (PEStm s) = name s
    translate (PEStm s, area) = translate (s, area)
    -- TODO: Other ProgramEntry types

instance AstNode Statement where
    translate (StmExpr e, area) = translate (e, area)
    -- TODO: Other Statement types

instance AstNode Expression where
    name (BinOp {}) = "BinaryExpr"
    name (IntLit _) = "IntLit"

{-
    translate (node@(BinOp op lexpr rexpr), area) = do
        clauses <- getClauses "BinaryExpr"
        sOp <- iOpSym op
        tOp <- translate op
        lOp <- iOpLong op
        left <- subexpr $ translate lexpr
        right <- subexpr $ translate rexpr

        parens <- chooseL sInSubExpr (bracket' '(' ')') id
        let vars = M.fromList [("opSymbol", sOp), ("opText", tOp)
                     , ("opTextLong", lOp), ("left", left), ("right", right)]
            conds = []
            cconds = M.empty
            translation = parens $ insertClauses clauses vars conds cconds
        result node translation
            
      where
        subexpr = localS (sInSubExpr .~ True)
-}

    translate (node@(IntLit val), area) = do
        clauses <- getClauses "IntLit"
        let vars = M.fromList [("value", OCString . T.pack $ show val)]
        result node area $ insertClauses clauses vars [] M.empty

    translate (anyNode, area) = do
        clauses <- getClauses $ name anyNode
        parens <- chooseM needsParens (bracket' '(' ')') id
        let runSubexpr = localS (sInSubExpr .~ True <<<
                            sPrevExprType .~ name anyNode) :: TL a -> TL a
        translateExpr anyNode area clauses parens runSubexpr
      where
        needsParens = (&&) <$> use sInSubExpr <*> ((name anyNode ==) <$>
                                                    use sPrevExprType)

translateExpr :: Expression -> Area -> [Clause] -> ([OutputClause] -> [OutputClause]) -> (forall a. TL a -> TL a)
                            -> TL OutputClause
translateExpr node@(BinOp op lexpr rexpr) area clauses parens runSubexpr = do
    tOp <- translate op
    left <- runSubexpr $ translate lexpr
    right <- runSubexpr $ translate rexpr
    let vars = M.fromList [("operation", AV tOp)
                 , ("left", AV left), ("right", AV right)]
        translation = parens $ insertClauses' clauses vars
    result node area translation
    


instance AstNode InfixOp where
    translate (node, area) = do
        node_ <- eFromJust $ M.lookup node translations
        clauses <- getClauses node_
        result node area $ insertClauses' clauses M.empty
      where
        translations = M.fromList [(Plus, "add"), (Minus, "subtract")
            , (Mult, "multiply"), (Div, "divide"), (Mod, "modulo")
            , (LOr, "logicalOr"), (LAnd, "logicalAnd"), (BOr, "bitOr")
            , (BAnd, "bitAnd"), (Xor, "xor"), (RShift, "rShift")
            , (LShift, "lShift"), (RUShift, "ruShift")]

class HasPrecedence a where
    precedence :: a -> Int

instance HasPrecedence InfixOp where
    precedence op = fromJust $ lookup op precedences
      where precedences =
              [(Mult, 4), (Div, 4), (Mod, 4), (Plus, 5), (Minus, 5), (RShift, 6)
              , (RUShift, 6), (LShift, 6), (Gt, 7), (Lt, 7), (GtEq, 7)
              , (LtEq, 7), (Equal, 8), (NEqual, 8), (BAnd, 9), (Xor, 10)
              , (BOr, 11), (LAnd, 12), (LOr, 13)]
