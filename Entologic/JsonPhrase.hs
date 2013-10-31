
import Entologic.Phrase

instance FromJSON Phrase where
    parseJSON (Object obj) =
        Phrase <$> obj .: "node"
               <*> obj .: "default"
               <*> obj .: pLangs

instance FromJSON PPhrase where
    parseJSON (Object obj) =
        PPhrase <$> obj .: "lang"
                <*> obj .: "nlangs"

instance FromJSON NPhrase where
    parseJSON (Object obj) =
        NPhrase <$> obj .: "nlang"
                <*> obj .: "clauses"

instance FromJSON Clause where
    parseJSON (String s) = return $ DefClause s 
    parseJSON (Object obj) =
        CondClause <$> obj .: "condition"
                   <*> obj .: "clause"

instance FromJSON 
