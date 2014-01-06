
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Entologic.Phrase.Json where

import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Control.Lens hiding ((.=))

import Text.Read (readMaybe)

import Entologic.Phrase
import Entologic.Error

readPhrases :: FilePath -> IO Phrases
readPhrases path = do
    json <- L.readFile path
    either (return . error) return $ eitherDecode json

{-
instance FromJSON (M.Map T.Text Phrase) where
    parseJSON (Array ar) = do
        phrases <- mapM parseJSON $ V.toList ar
        return $ foldl insert M.empty phrases
      where
        insert :: M.Map T.Text Phrase -> Phrase -> M.Map T.Text Phrase
        insert map phrase = M.insert (phrase ^. phNode) phrase map
-}

instance FromJSON Phrase where
    parseJSON o@(Object obj) =
        Phrase <$> obj .: "node"
               <*> (pphrase "default" =<< obj .: "default")
               <*> obj .: "plangs"

pphrase name (Object obj) = PPhrase name <$> (sphrase "en" =<< obj .: "en")
                                         <*> obj .: "slangs"

instance FromJSON PPhrase where
    parseJSON (Object obj) =
        PPhrase <$> obj .: "lang"
                <*> (sphrase "en" =<< obj .: "en")
                <*> obj .: "slangs"

sphrase name (Object obj) = SPhrase name <$> obj .: "clauses"

instance FromJSON SPhrase where
    parseJSON (Object obj) =
        SPhrase <$> obj .: "slang"
                <*> obj .: "clauses"

instance FromJSON Clause where
    parseJSON a@(Array _) = DefClause <$> parseJSON a
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
    parseJSON (String s) = mFromJust . readMaybe $ T.unpack s


instance ToJSON ClauseCond where
    toJSON (Present not attr) =
        object [ "condition" .= s "present", "not" .= not, "attribute" .= attr ]
    toJSON (Comp not comp attr val) =
        object [ "condition" .= s "present", "not" .= s "not"
               , "comparison" .= show comp, "attribute" .= attr
               , "value" .= val ]

instance ToJSON Clause where
    toJSON (DefClause clauses) = toJSON clauses

instance ToJSON SPhrase where
    toJSON (SPhrase lang clauses) = object [ "slang" .= lang
                                           , "clauses" .= clauses ]

instance ToJSON PPhrase where
    toJSON (PPhrase lang en slangs) = object [ "lang" .= lang, "en" .= en
                                                , "slangs" .= slangs ]

instance ToJSON Phrase where
    toJSON (Phrase node def langs) = object [ "node" .= node, "default" .= def
                                            , "plangs" .= langs ]

