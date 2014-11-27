{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE ViewPatterns #-}
module LogAnalysis where 
import Log 
import Text.Read

parseMessage :: String -> LogMessage 
parseMessage l = helper $ words l where
    helper ("I" : (readMaybe -> Just i) : xs) = LogMessage Info i (unwords xs)
    helper ("W" : (readMaybe -> Just i) : xs) = LogMessage Warning i (unwords xs) 
    helper ("E" : (readMaybe -> Just i) : (readMaybe -> Just j) : xs) = LogMessage (Error i) j (unwords xs) 
    helper _ = Unknown l 

mStamp :: LogMessage -> TimeStamp
mStamp (LogMessage _ timestamp _) = timestamp
mStamp _ = 1000  

mType :: LogMessage -> MessageType 
mType (LogMessage messagetype _ _) = messagetype 
mType l = Unknown l 

mcompare :: LogMessage -> LogMessage -> Ordering
mcompare a b = case (mType a, mType b) of
    (Info, Warning) -> GT 
    (Info, Error i) -> GT 
    (Warning, Error i) -> GT 
    (Warning, Info) -> LT 
    (Error i, Info) -> LT 
    (Error i, Warning) -> LT 
    (_, "Unknown String") -> GT 
    ("Unknown Sttring", _) -> LT 
    (_, _) -> compare (mStamp a) (mStamp b) 


insert :: LogMessage -> MessageTree -> MessageTree 
insert message Leaf = Node Leaf message Leaf 
insert message (Node lmessage mmessage rmessage) = case mcompare message mmessage of
    LT -> Node (insert message lmessage) mmessage rmessage
    EQ -> Node lmessage mmessage (insert message rmessage) 
    GT -> Node lmessage mmessage (insert message rmessage) 
