
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Entologic.JsonPhrase where

import Entologic.Phrase
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Lens

readPhrases :: FilePath -> IO Phrases
readPhrases path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json

instance FromJSON (M.Map T.Text Phrase) where
    parseJSON (Array ar) = do
        phrases <- mapM parseJSON $ V.toList ar
        return $ foldl insert M.empty phrases
      where
        insert :: M.Map T.Text Phrase -> Phrase -> M.Map T.Text Phrase
        insert map phrase = M.insert (phrase ^. phNode) phrase map

instance FromJSON Phrase where
    parseJSON (Object obj) =
        Phrase <$> obj .: "node"
               <*> obj .: "default"
               <*> obj .: "pLangs"

instance FromJSON PPhrase where
    parseJSON (Object obj) =
        PPhrase <$> obj .: "lang"
                <*> obj .: "en"
                <*> obj .: "nlangs"

instance FromJSON SPhrase where
    parseJSON (Object obj) =
        SPhrase <$> obj .: "nlang"
                <*> obj .: "clauses"

instance FromJSON Clause where
    parseJSON (String s) = return $ DefClause s
    parseJSON (Object obj) =
        CondClause <$> obj .: "condition"
                   <*> obj .: "clause"

instance FromJSON ClauseCond where
    parseJSON (Object obj) = do
        typ <- obj .: "condition"
        case typ :: T.Text of
          "present" -> Present <$> obj .: "not"
                               <*> obj .: "attribute"
          "comp" -> Comp <$> obj .: "not"
                         <*> obj .: "comparison"
                         <*> obj .: "attribute"
                         <*> obj .: "value"

instance FromJSON Ordering where
    parseJSON (String s) = return . read $ T.unpack s
