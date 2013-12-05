
{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             GeneralizedNewtypeDeriving,
             TemplateHaskell,
             MultiParamTypeClasses,
             InstanceSigs,
             TypeSynonymInstances,
             FlexibleInstances,
             OverloadedStrings #-}

module Entologic.Ast where

import Data.Text
import Control.Monad.IO.Class

import Entologic.Phrase
import Entologic.Output

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Trans.State.Lazy(StateT(..), evalStateT)
import Control.Monad.State.Class
import Control.Monad.Trans.Reader(ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Error(ErrorT(..))
import Control.Monad.Trans.Class
import Control.Lens.TH

data TLState = TLState { _sInSubExpr :: Bool
                       }
data TLInfo = TLInfo { _tlPhrases :: Phrases
                     , _tlPLang :: PLang
                     , _tlSLang :: SLang
                     }
              deriving (Eq, Ord, Show)

$(makeLenses ''TLInfo)
$(makeLenses ''TLState)

type WebTranslator = IO

type TLError = String

newtype TL a = TL { unTL :: (ErrorT TLError (ReaderT TLInfo (StateT TLState WebTranslator)) a) }
               deriving (Functor, Applicative, Monad)

instance MonadState TLState TL where
    get = TL get
    put = TL . put

instance MonadReader TLInfo TL where
    ask = TL ask
    local f (TL m) = TL $ local f m

instance MonadError TLError TL where
    throwError = TL . throwError
    catchError (TL m) f = TL $ catchError m (unTL . f)

runTL :: TLInfo -> TLState -> TL a -> IO (Either TLError a)
runTL info s tl = (flip evalStateT s) . (flip runReaderT info) . runErrorT . unTL $ tl


data AstMeta = AstMeta { mPLang :: PLang, mSLang :: SLang }
               deriving (Show)

class AstNode a where
    translate :: a -> TL OutputNode
    translate = undefined
    name :: a -> Text
    name = const ""

data NInf = NInf {lineNo :: Int}
            deriving (Show)

data ProgramEntry = PEFunc Function
                  | PECls Class
                  | PEStm Statement
                  deriving (Show)

data Program = Program { pEntries :: [ProgramEntry] }
               deriving (Show)

data Type = StringT Text
--          | forall a. (ASTNode a, Show a) => LSType a
            deriving (Show, Ord, Eq)


data LSAny = forall a. (AstNode a, Show a) => LSAny a

instance Show LSAny where
    show _ = "LSAny"

data Class = Class { cName :: String
                   , cSuperCls :: Maybe Type
                   , cMembers :: [Member]
                   }
             deriving (Show)

data Member = MFunc Function
            | MField Field
              deriving (Show)

data Field = Field
             deriving (Show)

data Function = Function { fName :: String
                         , fRTyp :: (Maybe Type)
                         , fParams :: [ParamDecl]
                         , fBody :: Body
                         , fExtra :: (Maybe LSAny) 
                         }
                deriving (Show)

data ParamDecl = ParamDecl { pName :: Text, pType :: (Maybe Type) }
                 deriving (Show, Ord, Eq)

data Body = Body [Statement]
            deriving (Show, Ord, Eq)

data Statement = VarDecl { vdType :: Type
                         , vdName :: Text
                         , vdInit :: Maybe Expression
                         }
               | StmExpr Expression
                 deriving (Show, Ord, Eq)

data VarRef = StringV Text
            | DottedV [Text]
              deriving (Show, Ord, Eq)

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
               deriving (Show, Ord, Eq)

data PrefixOp = Neg
              | BInv
              | PreInc
              | PreDec
                deriving (Show, Ord, Eq)

data PostfixOp = PostInc
               | PostDec
                 deriving (Show, Ord, Eq)

data Expression = Assign VarRef Expression
                | OpAssign VarRef InfixOp Expression
                | BinOp InfixOp Expression Expression
                | IntLit Integer
                | StringLit Text
                | PreOp PrefixOp Expression
                | PostOp PostfixOp Expression
                  deriving (Show, Ord, Eq)
