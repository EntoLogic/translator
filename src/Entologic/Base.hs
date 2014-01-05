
{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             GeneralizedNewtypeDeriving#-}

module Entologic.Base where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.Error
import Control.Monad.Error.Class

import Control.Applicative
import Control.Lens.TH

import Data.Text
import qualified Data.Map as M

type PLang = Text
type SLang = Text

type SPhrases = M.Map SLang SPhrase
type PPhrases = M.Map PLang PPhrase

data Phrase = Phrase { _phNode :: Text
                     , _phDefault :: PPhrase
                     , _phLangs :: PPhrases
                     }
              deriving (Ord, Eq, Show)

data PPhrase = PPhrase { _ppLang :: Text
                       , _pSEnglish :: SPhrase
                       , _pSLangs :: SPhrases
                       }
               deriving (Ord, Eq, Show)

data SPhrase = SPhrase { _spLang :: Text
                       , _spClauses :: [Clause]
                       }
               deriving (Ord, Eq, Show)

data Clause = DefClause [Text]
            | CondClause { cond :: ClauseCond, clause :: [Text] }
              deriving (Ord, Eq, Show)

data ClauseCond = Present { ccNot :: Bool
                          , ccAttribute :: Text
                          }
                | Comp { ccNot :: Bool
                       , ccComparison :: Ordering
                       , ccAttribute :: Text
                       , ccValue :: Int
                       }
                  deriving (Ord, Eq, Show)

type Phrases = M.Map Text Phrase

$(makeLenses ''Phrase)
$(makeLenses ''PPhrase)
$(makeLenses ''SPhrase)

data TLState = TLState { _sInSubExpr :: Bool
                       , _sPrevExprType :: Text
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

newtype TL a = TL { unTL :: (ErrorT TLError (ReaderT TLInfo
                                (StateT TLState WebTranslator)) a) }
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
runTL info s tl = (flip evalStateT s) . (flip runReaderT info) . runErrorT .
                     unTL $ tl


data Area = Area { _start :: Maybe Location, _end :: Maybe Location }
            deriving (Eq, Ord, Show)

data Location = Location { _line :: Int, _col :: Int }
                deriving (Eq, Ord, Show)

tupleM :: Monad m => (m a, m b) -> m (a, b)
tupleM (ma, mb) = do
    a <- ma
    b <- mb
    return (a, b)
